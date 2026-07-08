# Cellar Privacy Policy

The purpose of this Policy is to inform users who use cellar (the Application) about what personal data may be processed in the Application, as well as about the purposes of the processing and the manner in which the data are used, and about the related rights of the Users. The personal data administrator (hereinafter referred to as Administrator) protects the privacy of the Users and ensures the security of the data provided by them. The Administrator shall observe the principles of personal data processing and shall apply technical and organizational measures ensuring data security and processing in accordance with the provisions of law. Users' personal data are always processed in accordance with the applicable legal provisions, including in particular the Regulation of the European Parliament and of the Council on the protection of individuals with regard to the processing of personal data and on the free movement of such data and repealing Directive 95/46/EC (GDPR).

## Who is the controller?

The Controller is Virtus Lab Ltd (Sp. z o.o.) with registered office in Rzeszów (35-211), Poland, ul. Zofii Nałkowskiej 23 street, National Court Register entry No. KRS 0000349785, Tax Id. No. (NIP) 5170312965, Industry Id. No. (REGON) 180526627 (the Controller).

The Controller has appointed a Data Protection Officer (DPO) Mr. Mariusz Zajkiewicz, who can be contacted using the email address: dpo@virtuslab.com or by traditional mail to the company's address.

## Purposes and legal basis of data processing:

We may collect data that is required to provide the software, service or other information to you, including validating licenses and updates, providing technical support, reporting problems and errors, and other processing associated with the use of the software, service or information. We may use the data collected to develop and test products and services.

We collect the following data which in whole or in part may be considered personal data, the basis for processing the following data is consent as defined in article 6.1.a, GDPR.

- an anonymous installation identifier (a randomly-generated UUID stored locally in `~/.cellar/installation_id`, which the User can reset at any time with `cellar telemetry reset-id`),
- the name of the cellar subcommand invoked (e.g. `get`, `search`, `list-external`),
- the version of the Application,
- the operating system family reported by the JVM (e.g. `Mac OS X`, `Linux`); the operating system version is not collected,
- whether the command succeeded or failed, and — only in the case of failure — a coarse error category (`user` or `system`) and the class name of the underlying exception,
- the build tool detected in the User's project when cellar resolves a project classpath (one of: `Mill`, `Sbt`, `ScalaCli`),
- the duration and the name of a fixed, allow-listed set of internal operations (`cellar.command`, `symbol.resolve`, `build.classpath`, `coursier.fetch`, `tasty.context.init`).

The Application does **not** collect: Maven coordinates queried by the User, fully-qualified symbol names, search queries, project file paths, file contents, error messages or stack traces, the User's system locale, time zone, geographic location, or any keyboard or interaction data.

The User's IP address is briefly observed at the ingestion endpoint solely to enforce per-source rate limits; it is stripped before the request is forwarded to the telemetry storage pipeline and is not recorded in access logs.

The data collected in the telemetry process will be used to analyze the performance of the software, to improve its operation in order to adapt it to the needs and preferences of the Users.

Cellar telemetry data is collected and processed with your consent on infrastructure operated by the Controller and hosted by Hetzner Online GmbH (Germany, European Union), which acts as a subprocessor. All telemetry components (ingress, OpenTelemetry collector, trace storage, metrics storage, and dashboards) are self-hosted by the Controller on that infrastructure; no third-party analytics, customer-data, or telemetry SaaS providers receive Users' telemetry data.

Without the above personal data, it will not be possible to provide certain Services to the User.

in order to exchange email correspondence: The personal data are processed for the purpose of communication with the User on the basis of the legitimate interests pursued by the Administrator as defined in article 6.1.f, GDPR. Personal data are provided voluntarily, but their provision is necessary to receive a response from the Administrator. In this case, the personal data are processed due to the legitimate interests of the Controller. The Controller's legitimate interest is to communicate with the individual who requests a response from the Controller. As its legitimate interests, in accordance with article 6.1.f, GDPR, the Controller also takes into account: the investigation and protection against claims, the prevention of fraud, statistics and analytics, ensuring the security of the ICT environment, the application of internal control systems and, in some cases, the direct marketing of its own services.

Recipients of personal data: Personal data may be processed by other service providers of the Administrator providing, among others, financial settlement, legal, advisory, consulting, archiving and IT services. Users' data will not be disclosed to third parties unless it is necessary and the User gives their consent to it or the obligation to disclose the data results from mandatory provisions of law, a valid court judgment or a final decision of a competent authority. The Administrator does not transfer any data to third countries outside the EEA.

## What is profiling and is any data in the App subject to profiling?

Profiling consists in any form of automated processing of personal data evaluating personal aspects relating to a natural person, in particular for the purpose of analyzing or forecasting aspects relating to work performance, economic situation, health, personal preferences or interests, reliability or behaviour, location or movements of the data subject, where this produces legal effects concerning the data subject or significantly affects the data subject in a similar manner. The Administrator does not profile the User's sensitive data. To no extent is the data in the Application subject to profiling.

## How can personal data be changed?

The user has the right of access to the content of their personal data and the right to rectification and erasure, the right to restrict processing. The User has the right to object to the processing of personal data, which consists in particular in profiling. For this purpose, the User may contact the Administrator at the e-mail address: dpo@virtuslab.com. The User may also contact the Administrator in other ways, at their discretion, including orally and in writing.

## How does the Administrator protect personal data?

The Administrator secures the Users' data against unauthorized access, disclosure, change or destruction. In particular, the Administrator uses data encryption, physical security measures and verification in IT systems. In addition, the Administrator uses anti-virus software and firewalls. The Users' data can be accessed only by authorized persons obliged to maintain confidentiality, and by subcontractors who have concluded agreements with the Administrator for the subcontracting of personal data processing and who meet the security criteria specified therein.

## How long will personal data be processed?

Users' data will be processed for the duration of the Users' use of the Application. In the case of the provision of Services, personal data shall be processed for the duration of the provision of the Services. In the case of e-mail correspondence, personal data shall be processed for the period necessary to reply to the User. To a limited extent, personal data may also be processed after the expiry of the indicated deadlines, until the statute of limitations for possible claims or as long as it is possible or required under applicable law, e.g. for statistical purposes. After the expiry of the processing period, the personal data are permanently deleted or anonymized.

For telemetry data specifically, the Controller applies the following storage limits to give effect to the storage-limitation principle (Art. 5(1)(e) GDPR):

- Raw telemetry records — which carry the pseudonymous `installation.id` identifier — are retained for **30 days** in the Controller's trace store, after which they are deleted automatically.
- Pre-aggregated usage metrics derived from telemetry are retained for longer periods on the Controller's metrics store, but by construction they contain **no pseudonymous identifier** (`installation.id` is not promoted to a metric dimension) and therefore do not constitute personal data.

## Other rights of Users related to data processing

Users have the right to lodge a complaint with the President of the Office for Personal Data Protection if they consider that the processing of their personal data violates mandatory provisions of law.

This Policy shall be effective from the time of its publication on the Website.

## Manage consent

The Controller is Virtus Lab Ltd (Sp. z o.o.) with registered office in Rzeszów (35-211), Poland, ul. Zofii Nałkowskiej 23 street, National Court Register entry No. KRS 0000349785, Tax Id. No. (NIP) 5170312965, Industry Id. No. (REGON) 180526627 (the Controller).

The Controller has appointed a Data Protection Officer (DPO) Mr. Mariusz Zajkiewicz, who can be contacted using the email address: dpo@virtuslab.com or by traditional mail to the company's address.

## Application settings

Telemetry in cellar is opt-in. The first time the User runs the Application, a one-time notice is printed on standard error; the command runs normally and telemetry remains disabled until the User explicitly opts in. The User can change this setting at any time using the following commands:

```sh
cellar telemetry enable     # opt in
cellar telemetry disable    # opt out
cellar telemetry status     # show current status and installation ID
cellar telemetry reset-id   # generate a new anonymous installation ID
```

Telemetry can also be controlled through the `~/.cellar/cellar.conf` configuration file or via the `CELLAR_OTEL_ENABLED` and `CELLAR_OTEL_ENDPOINT` environment variables.

This Policy is effective from the moment of its publication on the Website. More about data processing at Virtus Lab Sp. z o.o. at https://virtuslab.com/privacy-policy/.
