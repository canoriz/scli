package scli

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"

	"github.com/BurntSushi/toml"
	"github.com/fsnotify/fsnotify"
	"gopkg.in/yaml.v3"
)

// LiveUpdateOpt is type restriction of L in File[T, L].
// EnableLiveUpdate and DisableLiveUpdate are two types implemented LiveUpdateOpt.
// This interface SHOULD NOT be implemented by users.
type LiveUpdateOpt interface {
	isWatched() bool
}

var (
	_ Parse     = &File[any, EnableLiveUpdate]{}
	_ Parse     = &File[any, DisableLiveUpdate]{}
	_ ParseOnce = &MarkOnce[*File[any, EnableLiveUpdate]]{}
)

var (
	EnableLU  LiveUpdateOpt = EnableLiveUpdate{}
	DisableLU LiveUpdateOpt = DisableLiveUpdate{}
)

// EnableLiveUpdate implements LiveUpdateOpt
type EnableLiveUpdate struct{}

func (EnableLiveUpdate) isWatched() bool { return true }

// DisableLiveUpdate implements LiveUpdateOpt
type DisableLiveUpdate struct{}

func (DisableLiveUpdate) isWatched() bool { return false }

// Load T from configuration file
// File[T, L] can be used with scli argument parser, or separately
// To enable live update, use File[T, scli.EnableLiveUpdate]
// To disable live update, use File[T, scli.DisableLiveUpdate]
type File[T any, L LiveUpdateOpt] struct {
	// go vet will warn if user try to copy instance.

	parsed atomic.Bool

	// Explaining why f.t is type *T, not T.
	// When live updating, other routine may still hold
	// reference to f.t. If f.T is type T, data race might happen
	//
	// If f.t is type T
	// x := f.Get()
	// f.t is updated by another routine
	// While updating f.t, if read x occurred, a concurrent read/write
	// will cause race or panic!
	//
	// If f.t is type *T
	// x := f.Get()
	// Another routine updates a new CopyT value and replace f.t with
	// pointer to it. Make sure Get() and Update() is does not occur
	// concurrently. Get() will return *(new pointer) while the old
	// pointer (variable x) still works
	//
	// File[T] does not introduce data race. if users hold two value
	// from Get() (and T{*R}, two T value contains same *R), then
	// read/write R concurrently, it's their problem. Because use T
	// instead of File[T] will still race.
	atomT atomic.Pointer[T]
	t     *T

	// these are for live-update
	liveUpdate L
	events     chan fsnotify.Event
}

// this is a generic unmarshal function
// json, yaml, toml all implemented this
type unmarshalFn func(data []byte, v any) error

// Parse data from file
func (f *File[T, L]) FromString(source string) error {
	if !f.parsed.CompareAndSwap(false, true) {
		// make sure this method is called only once
		panic("File[T,L].FromString() is called more than once")
	}

	err := f.fromString(source)
	if err != nil {
		return err
	}

	// watchChange starts only once
	// because FromString should be called only once
	if f.liveUpdate.isWatched() {
		f.events = make(chan fsnotify.Event, 2)
		f.watchChange(source)
	}
	return nil
}

func (f *File[T, L]) fromString(source string) error {
	content, err := os.ReadFile(source)
	if err != nil {
		return err
	}

	parseOrder := []unmarshalFn{
		json.Unmarshal, yaml.Unmarshal, toml.Unmarshal,
	}
	if strings.HasSuffix(source, ".yaml") || strings.HasSuffix(source, ".yml") {
		parseOrder = []unmarshalFn{yaml.Unmarshal}
	} else if strings.HasSuffix(source, ".json") {
		parseOrder = []unmarshalFn{json.Unmarshal}
	} else if strings.HasSuffix(source, ".toml") {
		parseOrder = []unmarshalFn{toml.Unmarshal}
	}

	value, err := parseByOrder[T](content, parseOrder)
	if err != nil {
		return err
	}

	if f.liveUpdate.isWatched() {
		f.atomT.Store(&value)
	} else {
		f.t = &value
	}
	return nil
}

// Example returns an example of config-file input
// Marker for Parse
func (f *File[T, L]) Example() string {
	return "config-file"
}

// Marker for ParseOnce
func (f *File[T, L]) statefulOrImpure() {}

// Get() returns the inner T instance
func (f *File[T, L]) Get() *T {
	if f.liveUpdate.isWatched() {
		t := f.atomT.Load()
		return t
	}
	return f.t
}

func (f *File[T, L]) watchChange(filename string) {
	configFile := filepath.Clean(filename)
	configDir, _ := filepath.Split(configFile)
	realConfigFile, _ := filepath.EvalSymlinks(filename)

	// we have to watch the entire directory to pick up renames/atomic saves in a cross-platform way
	watcher, err := fsnotify.NewWatcher()
	if err != nil {
		log.Printf("failed to create watcher: %s", err)
		os.Exit(1)
	}

	if err := watcher.Add(configDir); err != nil {
		log.Printf("watch add conf dir err %v", err)
		watcher.Close()
		return
	}

	go func(watcher *fsnotify.Watcher) {
		defer watcher.Close()
		for {
			select {
			case event, ok := <-watcher.Events:
				if !ok { // 'Events' channel is closed
					return
				}
				currentConfigFile, _ := filepath.EvalSymlinks(filename)
				// we only care about the config file with the following cases:
				// 1 - if the config file was modified or created
				// 2 - if the real path to the config file changed (eg: k8s ConfigMap replacement)
				if (filepath.Clean(event.Name) == configFile &&
					(event.Has(fsnotify.Write) || event.Has(fsnotify.Create))) ||
					(currentConfigFile != "" && currentConfigFile != realConfigFile) {
					realConfigFile = currentConfigFile
					err := f.fromString(filename)
					if err != nil {
						log.Printf("read config file error: %s", err) // TODO: better way of log
					}
					if f.liveUpdate.isWatched() {
						select {
						case f.events <- event:
						default:
							// if f.events blocks, discard this event
						}
					}
				} else if filepath.Clean(event.Name) == configFile && event.Has(fsnotify.Remove) {
					return
				}

			case err, ok := <-watcher.Errors:
				if ok { // 'Errors' channel is not closed
					log.Printf("watcher error: %s", err)
				}
				return
			}
		}
	}(watcher)
}

// UpdateEvents() returns a channel of fsnotify.Events.
// An event will be send to this channel once the file changes.
func (f *File[T, L]) UpdateEvents() <-chan fsnotify.Event {
	// Not using viper's onConfigChange callback because callbacks may run
	// concurrently in a uncontrolled way. For example: when change comes,
	// previous and this change's callback are running concurrently.
	// Channels don't.
	return f.events
}

type errList []error

func (el errList) Error() string {
	ret := []string{}
	for _, e := range el {
		ret = append(ret, fmt.Sprintf("[%s]", e.Error()))
	}
	return strings.Join(ret, " ")
}

func parseByOrder[T any](
	content []byte, parseOrder []unmarshalFn,
) (T, error) {
	var t T
	elist := []error{}
	for _, unmarshal := range parseOrder {
		err := unmarshal(content, &t)
		if err == nil {
			return t, nil
		} else {
			elist = append(elist, err)
		}
	}
	return t, errList(elist)
}
