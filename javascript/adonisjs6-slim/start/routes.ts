/*
|--------------------------------------------------------------------------
| Routes file
|--------------------------------------------------------------------------
|
| The routes file is used for defining the HTTP routes.
|
*/

import router from '@adonisjs/core/services/router'

router.get('/', (ctx) => ctx.response.noContent())

router.get('/user/:id', (ctx) => ctx.response.ok(ctx.params.id)

router.post('/user', (ctx) => ctx.response.noContent())
