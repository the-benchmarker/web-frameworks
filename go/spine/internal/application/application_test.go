package application

import "testing"

func TestBuildApplicationIsValid(t *testing.T) {
	app, options, err := BuildApplication()
	if err != nil {
		t.Fatalf("failed to build application: %v", err)
	}
	if err := app.Validate(options); err != nil {
		t.Fatalf("invalid application configuration: %v", err)
	}
}
