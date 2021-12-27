<?php

namespace Config;

use CodeIgniter\Config\BaseConfig;

/**
 * Stores the default settings for the ContentSecurityPolicy, if you
 * choose to use it. The values here will be read in and set as defaults
 * for the site. If needed, they can be overridden on a page-by-page basis.
 *
 * Suggested reference for explanations:
 *
 * @see https://www.html5rocks.com/en/tutorials/security/content-security-policy/
 */
class ContentSecurityPolicy extends BaseConfig
{
	//-------------------------------------------------------------------------
	// Broadbrush CSP management
	//-------------------------------------------------------------------------

	/**
	 * Default CSP report context
	 *
	 * @var boolean
	 */
	public $reportOnly = false;

	/**
	 * Specifies a URL where a browser will send reports
	 * when a content security policy is violated.
	 *
	 * @var string|null
	 */
	public $reportURI = null;

	/**
	 * Instructs user agents to rewrite URL schemes, changing
	 * HTTP to HTTPS. This directive is for websites with
	 * large numbers of old URLs that need to be rewritten.
	 *
	 * @var boolean
	 */
	public $upgradeInsecureRequests = false;

	//-------------------------------------------------------------------------
	// Sources allowed
	// Note: once you set a policy to 'none', it cannot be further restricted
	//-------------------------------------------------------------------------

	/**
	 * Will default to self if not overridden
	 *
	 * @var string|string[]|null
	 */
	public $defaultSrc = null;

	/**
	 * Lists allowed scripts' URLs.
	 *
	 * @var string|string[]
	 */
	public $scriptSrc = 'self';

	/**
	 * Lists allowed stylesheets' URLs.
	 *
	 * @var string|string[]
	 */
	public $styleSrc = 'self';

	/**
	 * Defines the origins from which images can be loaded.
	 *
	 * @var string|string[]
	 */
	public $imageSrc = 'self';

	/**
	 * Restricts the URLs that can appear in a page's `<base>` element.
	 *
	 * Will default to self if not overridden
	 *
	 * @var string|string[]|null
	 */
	public $baseURI = null;

	/**
	 * Lists the URLs for workers and embedded frame contents
	 *
	 * @var string|string[]
	 */
	public $childSrc = 'self';

	/**
	 * Limits the origins that you can connect to (via XHR,
	 * WebSockets, and EventSource).
	 *
	 * @var string|string[]
	 */
	public $connectSrc = 'self';

	/**
	 * Specifies the origins that can serve web fonts.
	 *
	 * @var string|string[]
	 */
	public $fontSrc = null;

	/**
	 * Lists valid endpoints for submission from `<form>` tags.
	 *
	 * @var string|string[]
	 */
	public $formAction = 'self';

	/**
	 * Specifies the sources that can embed the current page.
	 * This directive applies to `<frame>`, `<iframe>`, `<embed>`,
	 * and `<applet>` tags. This directive can't be used in
	 * `<meta>` tags and applies only to non-HTML resources.
	 *
	 * @var string|string[]|null
	 */
	public $frameAncestors = null;

	/**
	 * The frame-src directive restricts the URLs which may
	 * be loaded into nested browsing contexts.
	 *
	 * @var array|string|null
	 */
	public $frameSrc = null;

	/**
	 * Restricts the origins allowed to deliver video and audio.
	 *
	 * @var string|string[]|null
	 */
	public $mediaSrc = null;

	/**
	 * Allows control over Flash and other plugins.
	 *
	 * @var string|string[]
	 */
	public $objectSrc = 'self';

	/**
	 * @var string|string[]|null
	 */
	public $manifestSrc = null;

	/**
	 * Limits the kinds of plugins a page may invoke.
	 *
	 * @var string|string[]|null
	 */
	public $pluginTypes = null;

	/**
	 * List of actions allowed.
	 *
	 * @var string|string[]|null
	 */
	public $sandbox = null;
}
