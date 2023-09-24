import { rootModule } from '@ditsmod/core';
import { RouterModule } from '@ditsmod/router';
import { BodyParserModule } from '@ditsmod/body-parser';

import { HelloWorldController } from './hello-world.controller.js';

@rootModule({
  controllers: [HelloWorldController],
  imports: [RouterModule, BodyParserModule]
})
export class AppModule {}
