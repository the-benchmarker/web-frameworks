# Spine WFB 구현

공개 릴리스 `github.com/NARUBROWN/spine v0.5.2`를 사용하는 최소 HTTP 앱입니다.
모든 요청은 Spine의 라우팅, 생성자 주입, 인자 해석, 컨트롤러와 응답 처리를 거칩니다.
HTTP 어댑터인 Echo에 직접 라우트를 등록하거나 기본 HTTP 보호 설정을 변경하지 않습니다.

| 요청 | 상태 | 본문 |
| --- | --- | --- |
| `GET /` | 200 | 빈 본문 |
| `GET /user/:id` | 200 | 전달받은 `id` |
| `POST /user` | 200 | 빈 본문 |

기본 포트는 `3000`이며 `SPINE_PORT`로 변경할 수 있습니다.
데이터베이스, 메시지 브로커와 외부 서비스는 사용하지 않습니다.

이 디렉터리에서 `go mod download` 후 `spine contract check --json`과
`spine verify --json`으로 정적 계약과 Go 검증을 실행할 수 있습니다.

WFB 저장소 루트에서 공식 컨테이너 검증을 실행합니다.

```sh
bundle exec rake config
make -f go/spine/.Makefile build
make -f go/spine/.Makefile test
make -f go/spine/.Makefile unbuild
```

공통 언어 템플릿이 `.Dockerfile.net-http`와 `.Makefile`을 생성합니다.
`config.yaml`은 `go.sum`도 빌드에 포함하도록 지정합니다.
응답 계약 검증은 성능 측정과 구분하며, 자체 측정값을 공개 순위로 간주하지 않습니다.
