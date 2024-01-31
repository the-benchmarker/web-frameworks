/*
|--------------------------------------------------------------------------
| Routes file
|--------------------------------------------------------------------------
|
| The routes file is used for defining the HTTP routes.
|
*/

import router from '@adonisjs/core/services/router'

router.get('/', async () => {
  return '';
});
router.get('/user/:id', async ({ params }) => {
  return params.id;
});
router.post('/user', async () => {
  return '';
});