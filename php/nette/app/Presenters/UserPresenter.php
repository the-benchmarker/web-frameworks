<?php

declare(strict_types=1);

namespace App\Presenters;

use Nette\Application\Responses\TextResponse;
use Nette\Application\UI\Presenter;

final class UserPresenter extends Presenter
{
    public function actionDefault(string $id = null): void
    {
        if ($id) {
            $this->sendResponse(new TextResponse($id));
        }
        $this->sendResponse(new TextResponse(''));
    }
}
