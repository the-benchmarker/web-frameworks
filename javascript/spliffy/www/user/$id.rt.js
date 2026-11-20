import { declarative } from '@srfnstack/spliffy';

// GET "/user/:id" => 200 with "id" as body
export default {
  GET: declarative().writeParameterValue('id').end(''),
};
