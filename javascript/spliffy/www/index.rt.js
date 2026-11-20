import { declarative } from '@srfnstack/spliffy';

// GET "/" => 200 with empty body. A declarative response is compiled once and
// answered inside uWebSockets.js, no javascript runs per request.
export default {
  GET: declarative().end(''),
};
