package scli

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestArgs(t *testing.T) {
	type a struct {
		I     int     `flag:"aa" default:"45"`
		S     string  `flag:"bb" default:"fa"`
		I2    int     `flag:"dd" default:"55"`
		F     float64 `default:"0.14"`
		B     bool
		SS    []string `flag:"cc"`
		BS    []bool
		FS    []float64
		IS    []int
		SSDef []string `default:"v,v,v"`
		ISDef []int    `default:"1,4,2"`
		O1    [][]int
		O2    func()
	}
	var z a
	err := Build(&z).ParseArgs(
		strings.Split(
			"-aa 13 -bb 55ag -cc 1,2,3 "+
				"-BS true,1,F -B -FS -1.1,1.4,-6.7 -IS 1,-1",
			" ",
		),
	)
	if err != nil {
		t.Fatal(err)
	}
	expected := a{
		I:     13,
		S:     "55ag",
		I2:    55,
		F:     0.14,
		B:     true,
		SS:    []string{"1", "2", "3"},
		BS:    []bool{true, true, false},
		FS:    []float64{-1.1, 1.4, -6.7},
		IS:    []int{1, -1},
		SSDef: []string{"v", "v", "v"},
		ISDef: []int{1, 4, 2},
	}
	if diff := cmp.Diff(z, expected); diff != "" {
		t.Fatal(diff)
	}
}
