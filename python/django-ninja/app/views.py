from django.http import HttpResponse
from ninja import NinjaAPI


api = NinjaAPI()


@api.get("")
def index(request):
    return HttpResponse(status=200)


@api.get("user/{id}")
async def get_user(request, id: str):
    return HttpResponse(id)


@api.post("user")
async def create_user(request):
    return HttpResponse(status=200)
