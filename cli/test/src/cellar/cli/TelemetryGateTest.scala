package cellar.cli

import weaver.SimpleIOSuite

object TelemetryGateTest extends SimpleIOSuite:

  import TelemetrySubcommand.{parseChoice, shouldGateTelemetry, TelemetryChoice}

  pureTest("parseChoice maps numbers, letters, and full words to choices"):
    expect.all(
      parseChoice("1") == Some(TelemetryChoice.EnableProject),
      parseChoice("e") == Some(TelemetryChoice.EnableProject),
      parseChoice("enable") == Some(TelemetryChoice.EnableProject),
      parseChoice("2") == Some(TelemetryChoice.EnableGlobal),
      parseChoice("enable-global") == Some(TelemetryChoice.EnableGlobal),
      parseChoice("3") == Some(TelemetryChoice.DisableProject),
      parseChoice("d") == Some(TelemetryChoice.DisableProject),
      parseChoice("disable") == Some(TelemetryChoice.DisableProject),
      parseChoice("4") == Some(TelemetryChoice.DisableGlobal),
      parseChoice("g") == Some(TelemetryChoice.DisableGlobal),
      parseChoice("global") == Some(TelemetryChoice.DisableGlobal)
    )

  pureTest("parseChoice is case-insensitive and trims surrounding whitespace"):
    expect.all(
      parseChoice("  E  ") == Some(TelemetryChoice.EnableProject),
      parseChoice("\tGlobal\n") == Some(TelemetryChoice.DisableGlobal)
    )

  pureTest("parseChoice defaults empty input to disabling this project"):
    expect.all(
      parseChoice("") == Some(TelemetryChoice.DisableProject),
      parseChoice("   ") == Some(TelemetryChoice.DisableProject)
    )

  pureTest("parseChoice rejects unrecognized input"):
    expect.all(parseChoice("yes").isEmpty, parseChoice("x").isEmpty)

  pureTest("gate while unanswered and no marker present"):
    expect(shouldGateTelemetry(alreadyAnswered = false, markerAnswered = false))

  pureTest("never gate once the user has answered via config or a seen marker"):
    expect.all(
      !shouldGateTelemetry(alreadyAnswered = true, markerAnswered = false),
      !shouldGateTelemetry(alreadyAnswered = false, markerAnswered = true)
    )
