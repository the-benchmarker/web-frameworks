<?php

/**
 * SilverEngine  - PHP MVC framework
 *
 * @package   SilverEngine
 * @author    SilverEngine Team
 * @copyright 2015-2017
 * @license   MIT
 * @link      https://github.com/SilverEngine/Framework
 */

namespace Silver\Support;


class Git
{
    /**
     * @var string
     */
    private $branch;

    const GIT_MASTER = 'master';
    const GIT_DEVELOP = 'develop';

    const GIT_HOTFIX = 'hotfix';
    const GIT_FEATURE = 'feature';


    public function __construct(\SplFileObject $gitHeadFile)
    {
        $ref = explode("/", $gitHeadFile->current(), 3);

        $this->branch = rtrim($ref[2]);
    }


    public static function createFromGitRootDir($dir)
    {
        try {
            $gitHeadFile = new \SplFileObject($dir . '/.git/HEAD', 'r');
        } catch (\RuntimeException $e) {
            throw new \RuntimeException(sprintf('Directory "%s" is not a Git repository.', $dir));
        }

        return new static($gitHeadFile);
    }

    public static function test()
    {
        if(is_dir('.git')) {
            $stringfromfile = file('.git/HEAD', FILE_USE_INCLUDE_PATH);
            $firstLine = $stringfromfile[0]; //get the string from the array
            $explodedstring = explode("/", $firstLine, 3); //seperate out by the "/" in the string
            $branchname = $explodedstring[2]; //get the one that is always the branch name
            return $branchname;
        }
        return 'Git not included';

    }


    public function getName()
    {
        return $this->branch;
    }


    public function isBasedOnMaster()
    {
        return $this->getFlowType() === self::GIT_HOTFIX || $this->getFlowType() === self::GIT_MASTER;
    }


    public function isBasedOnDevelop()
    {
        return $this->getFlowType() === self::GIT_FEATURE || $this->getFlowType() === self::GIT_DEVELOP;
    }


    private function getFlowType()
    {
        $name = explode('/', $this->branch);

        return $name[0];
    }
}