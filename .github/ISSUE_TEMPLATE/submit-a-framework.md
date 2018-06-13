---
name: Submit a framework
about: Request a framework addition

---

+ All frameworks **SHOULD** follow this rules :

| HTTP   | Route       | Status code | Response body         |
| ------ | ----------- | ----------- | --------------------- |
| `GET`  | `/`         | `200`       | **Empty**             |
| `GET`  | `/user/:id` | `200`       | **id** given as param |
| `POST` | `/user`     | `200`       | **Empty**

+ All framework **SHOULD** contain a `Dockerfile`

+ All framework **SHOULD** be referenced in :
   + `Makefile`, a target group for the language, and a target for the framework
   + `neph.yaml`, a target group for the language, and a target for the framework
   + `benchmarker.cr`, a hash for language containing all frameworks an repository information
