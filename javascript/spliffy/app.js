import spliffy, { moduleDirname } from '@srfnstack/spliffy';
import path from 'path';

spliffy({
  routeDir: path.join(moduleDirname(import.meta.url), 'www'),
  port: 3000,
  logAccess: false,
});
