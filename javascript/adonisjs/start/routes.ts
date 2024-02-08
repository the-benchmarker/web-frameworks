/*
|--------------------------------------------------------------------------
| Routes file
|--------------------------------------------------------------------------
|
| The routes file is used for defining the HTTP routes.
|
*/

import router from '@adonisjs/core/services/router'

router.get('/', '#controllers/application_controller.index')

router.get('/user/:id', '#controllers/users_controller.show')

router.post('/user', '#controllers/users_controller.create')
