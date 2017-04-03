class Roda
  # The major version of Roda, updated only for major changes that are
  # likely to require modification to Roda apps.
  RodaMajorVersion = 2

  # The minor version of Roda, updated for new feature releases of Roda.
  RodaMinorVersion = 24

  # The patch version of Roda, updated only for bug fixes from the last
  # feature release.
  RodaPatchVersion = 0

  # The full version of Roda as a string.
  RodaVersion = "#{RodaMajorVersion}.#{RodaMinorVersion}.#{RodaPatchVersion}".freeze
end
