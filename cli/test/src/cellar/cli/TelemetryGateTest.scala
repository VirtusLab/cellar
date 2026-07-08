package cellar.cli

import munit.CatsEffectSuite

class TelemetryGateTest extends CatsEffectSuite:

  import TelemetrySubcommand.{parseChoice, shouldGateTelemetry, TelemetryChoice}

  test("parseChoice maps numbers, letters, and full words to choices"):
    assertEquals(parseChoice("1"), Some(TelemetryChoice.EnableProject))
    assertEquals(parseChoice("e"), Some(TelemetryChoice.EnableProject))
    assertEquals(parseChoice("enable"), Some(TelemetryChoice.EnableProject))
    assertEquals(parseChoice("2"), Some(TelemetryChoice.EnableGlobal))
    assertEquals(parseChoice("enable-global"), Some(TelemetryChoice.EnableGlobal))
    assertEquals(parseChoice("3"), Some(TelemetryChoice.DisableProject))
    assertEquals(parseChoice("d"), Some(TelemetryChoice.DisableProject))
    assertEquals(parseChoice("disable"), Some(TelemetryChoice.DisableProject))
    assertEquals(parseChoice("4"), Some(TelemetryChoice.DisableGlobal))
    assertEquals(parseChoice("g"), Some(TelemetryChoice.DisableGlobal))
    assertEquals(parseChoice("global"), Some(TelemetryChoice.DisableGlobal))

  test("parseChoice is case-insensitive and trims surrounding whitespace"):
    assertEquals(parseChoice("  E  "), Some(TelemetryChoice.EnableProject))
    assertEquals(parseChoice("\tGlobal\n"), Some(TelemetryChoice.DisableGlobal))

  test("parseChoice defaults empty input to disabling this project"):
    assertEquals(parseChoice(""), Some(TelemetryChoice.DisableProject))
    assertEquals(parseChoice("   "), Some(TelemetryChoice.DisableProject))

  test("parseChoice rejects unrecognized input"):
    assertEquals(parseChoice("yes"), None)
    assertEquals(parseChoice("x"), None)

  test("gate while unanswered and no marker present"):
    assert(shouldGateTelemetry(alreadyAnswered = false, markerAnswered = false))

  test("never gate once the user has answered via config or a seen marker"):
    assert(!shouldGateTelemetry(alreadyAnswered = true, markerAnswered = false))
    assert(!shouldGateTelemetry(alreadyAnswered = false, markerAnswered = true))
