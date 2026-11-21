/// The media type every JSON answer carries.
///
/// The contract compares the response `Content-Type` against this exact
/// value, so it is written explicitly instead of relying on the default
/// `text/html; charset=utf-8`.
pub(crate) const JSON_CONTENT_TYPE: &str = "application/json";

/// The media type of a JSON request body.
pub(crate) const JSON_MEDIA_TYPE: &str = "application/json";

/// The legacy media type that also names a JSON body.
pub(crate) const JSON_LEGACY_MEDIA_TYPE: &str = "text/x-json";

/// The legacy media type that also names a JSON body.
pub(crate) const JSON_REQUEST_MEDIA_TYPE: &str = "application/jsonrequest";

/// The media type of a multipart request body.
pub(crate) const MULTIPART_FORM_DATA_MEDIA_TYPE: &str = "multipart/form-data";

/// The JSON value of an absent byte sample.
pub(crate) const JSON_NULL: &str = "null";
