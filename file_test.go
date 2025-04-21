package scli

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

type testConfig struct {
	Foo string
}

func TestFile(t *testing.T) {
	const confFile = "config1.json"
	var a File[testConfig, DisableLiveUpdate]
	os.WriteFile(confFile, []byte(`{"foo": "bar"}`), 0o640)
	defer os.Remove(confFile)
	a.FromString(confFile)
	assert.Equal(t, &testConfig{Foo: "bar"}, a.Get())
}

func TestByteFile(t *testing.T) {
	const confFile = "config1"
	var a File[[]byte, DisableLiveUpdate]
	os.WriteFile(confFile, []byte{0xa, 0xd, 0xb, 0xc}, 0o640)
	defer os.Remove(confFile)
	a.FromString(confFile)
	assert.Equal(t, &[]byte{0xa, 0xd, 0xb, 0xc}, a.Get())
}

func TestFileLiveUpdate(t *testing.T) {
	const confFile = "config2.json"
	var a File[testConfig, EnableLiveUpdate]
	// initial content
	os.WriteFile(confFile, []byte(`{"foo": "bar"}`), 0o640)
	defer os.Remove(confFile)

	a.FromString(confFile)
	oldPtr := a.Get()
	assert.Equal(t, &testConfig{Foo: "bar"}, oldPtr, "test init content")

	os.WriteFile(confFile, []byte(`{"foo": "baz"}`), 0o640)
	<-a.UpdateEvents() // wait update done
	// test new value
	assert.Equal(t, &testConfig{Foo: "baz"}, a.Get(), "test new value")
	// verify old value still valid
	assert.Equal(t, &testConfig{Foo: "bar"}, oldPtr, "test old value")
}

func TestByteFileLiveUpdate(t *testing.T) {
	const confFile = "config2"
	var a File[[]byte, EnableLiveUpdate]
	// initial content
	os.WriteFile(confFile, []byte{0xa, 0xb}, 0o640)
	defer os.Remove(confFile)

	a.FromString(confFile)
	oldPtr := a.Get()
	assert.Equal(t, &[]byte{0xa, 0xb}, oldPtr, "test init content")

	os.WriteFile(confFile, []byte{0xa, 0xc}, 0o640)
	<-a.UpdateEvents() // wait update done
	// test new value
	assert.Equal(t, &[]byte{0xa, 0xc}, a.Get(), "test new value")
	// verify old value still valid
	assert.Equal(t, &[]byte{0xa, 0xb}, oldPtr, "test old value")
}

func TestFileLiveUpdateFirstInvalidThenValid(t *testing.T) {
	const confFile = "invalid.json"
	var a File[testConfig, EnableLiveUpdate]
	// initial content
	os.WriteFile(confFile, []byte(`{"foo": "bar"`), 0o640)
	defer os.Remove(confFile)

	a.FromString(confFile)
	oldPtr := a.Get()
	assert.Nilf(t, oldPtr, "test init invalid content")

	os.WriteFile(confFile, []byte(`{"foo": "baz"}`), 0o640)
	<-a.UpdateEvents() // wait update done
	// test new value
	assert.Equal(t, &testConfig{Foo: "baz"}, a.Get(), "test new value")
	// verify old value still valid
	assert.Nilf(t, oldPtr, "test old invalid value")
}
