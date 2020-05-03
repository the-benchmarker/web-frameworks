<?php

declare(strict_types=1);

namespace App\Presenters;

use Nette\Application\Responses\TextResponse;
use Nette\Application\UI\Presenter;

final class HomepagePresenter extends Presenter
{
    public function actionDefault(): void
    {
        $this->sendResponse(new TextResponse(''));
    }
}
