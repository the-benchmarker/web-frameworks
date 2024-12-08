import { rootModule } from '@ditsmod/core';
import { RoutingModule } from '@ditsmod/routing';
import { BodyParserModule } from '@ditsmod/body-parser';
import { HelloWorldController } from './hello-world.controller.js';

@rootModule({
  imports: [RoutingModule, BodyParserModule],
  controllers: [HelloWorldController],
})
export class AppModule {}
