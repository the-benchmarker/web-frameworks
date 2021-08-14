package main

import "github.com/cnotch/apirouter"

	r := apirouter.New(
		apirouter.API("GET","/",func(w http.ResponseWriter, r *http.Request, ps apirouter.Params){
			w.Write([]byte(""))
		}),
		apirouter.API("GET","/user/:id", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			 w.Write(ps.ByName("id")
		}),
		apirouter.API("POST","/user/:id", func(w http.ResponseWriter, r *http.Request, ps apirouter.Params) {
			w.Write([]byte(""))
	   })
	)
	
	
http.ListenAndServe(":8080", r)