import { AnyObj, Router, rootModule } from '@ditsmod/core';
import { RouterModule } from '@ditsmod/router';
import { BodyParserModule } from '@ditsmod/body-parser';
import { parse, Headers } from 'get-body';

@rootModule({ imports: [RouterModule, BodyParserModule] })
export class AppModule {
  constructor(router: Router) {
    router
      .on('GET', '/', async (nodeReq, nodeRes) => {
        nodeRes.end();
      })
      .on('GET', '/user/:id', async (nodeReq, nodeRes, aPathParams) => {
        const pathParams: AnyObj = {};
        aPathParams.forEach((param) => (pathParams[param.key] = param.value));
        nodeRes.end(pathParams.id);
      })
      .on('POST', '/user', async (nodeReq, nodeRes) => {
        await parse(nodeReq, nodeReq.headers as Headers);
        nodeRes.end();
      });
  }
}
