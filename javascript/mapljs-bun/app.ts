import { router, jitc } from '@mapl/app';

const app = router()
   .get('/', () => '')
   .post('/user', () => '')
   .get('/user/*', (params) => params[0]);

// Port 3000
export default await jitc(app);
