from django.shortcuts import render
from django.http import HttpResponse
from django.views.decorators.csrf import csrf_exempt

def index(request):
    return HttpResponse(status=200)

def get_user(request, id):
    return HttpResponse(id)

@csrf_exempt
def create_user(request):
    return HttpResponse(status=200)
