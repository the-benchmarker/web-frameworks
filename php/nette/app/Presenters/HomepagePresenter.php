<?php

declare(strict_types=1);

namespace App\Presenters;

use Nette\Application\IPresenter;
use Nette\Application\IResponse;
use Nette\Application\Request;
use Nette\Application\Responses\TextResponse;

final class HomepagePresenter implements IPresenter
{
    public function run(Request $request): IResponse
    {
        return new TextResponse('');
    }
}
