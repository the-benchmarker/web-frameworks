from django.http import HttpResponse


def index(request):
    return HttpResponse(status=200)


def get_user(request, id):
    return HttpResponse(id)


def create_user(request):
    return HttpResponse(status=200)
