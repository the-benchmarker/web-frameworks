import { declarative } from '@srfnstack/spliffy';

// POST "/user" => 200 with empty body
export default {
  POST: declarative().end(''),
};
