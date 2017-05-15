#!/usr/bin/env python3.5
# -*- coding: utf-8 -*-


from japronto import Application


def index(request):
    return request.Response(text='')


def create_user(request):
    return request.Response(text='')


def get_user(request):
    return request.Response(text=str(request.match_dict['id']))


app = Application()
app.router.add_route('/', index)
app.router.add_route('/user', create_user, 'POST')
app.router.add_route('/user/{id}', get_user, 'GET')

app.run(debug=False, port=3000)
