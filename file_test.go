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
	const confFile = "config.json"
	var a File[testConfig, DisableLiveUpdate]
	os.WriteFile(confFile, []byte(`{"foo": "bar"}`), 0o640)
	a.FromString(confFile)
	assert.Equal(t, &testConfig{Foo: "bar"}, a.Get())
}

func TestFileLiveUpdate(t *testing.T) {
	const confFile = "config.json"
	var a File[testConfig, EnableLiveUpdate]
	// initial content
	os.WriteFile(confFile, []byte(`{"foo": "bar"}`), 0o640)

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
