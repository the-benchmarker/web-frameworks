from panther import Panther
from panther.app import API


@API(methods=['GET'])
async def index():
    return ''


@API(methods=['GET'])
async def get_user(id: str):
    return id


@API(methods=['POST'])
async def create_user() -> str:
    return ''


urls = {
    '/': index,
    '/user/<id>': get_user,
    '/user': create_user
}

app = Panther(__name__, configs=__name__, urls=urls)
