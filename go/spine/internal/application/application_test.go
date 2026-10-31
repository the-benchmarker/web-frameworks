package application

import "testing"

func TestBuildApplicationIsValid(t *testing.T) {
	app, options, err := BuildApplication()
	if err != nil {
		t.Fatalf("애플리케이션 구성에 실패했습니다: %v", err)
	}
	if err := app.Validate(options); err != nil {
		t.Fatalf("애플리케이션 설정이 유효하지 않습니다: %v", err)
	}
}
