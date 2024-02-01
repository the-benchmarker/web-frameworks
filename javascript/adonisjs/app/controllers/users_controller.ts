import type { HttpContext } from '@adonisjs/core/http'

export default class UsersController {
    async show({ params }: HttpContext) {
        return params.id;
    }
    async create() {
        return '';
    }
}