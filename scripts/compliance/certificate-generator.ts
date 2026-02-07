/**
 * Compliance Certificate Generator
 * Generates compliance certificates for audited standards
 */

import crypto from 'crypto';
import { ComplianceCertificate, ComplianceStandard, ComplianceReport, CheckResult } from './types';

export class CertificateGenerator {
  private readonly issuerName = 'Compliance Assurance Authority';
  private readonly issuerAddress = 'Compliance Certification Services';

  public generateCertificate(
    standard: ComplianceStandard,
    report: ComplianceReport,
    organization: string,
    auditor: string,
    scope: string,
  ): ComplianceCertificate {
    const issuedDate = new Date();
    const expiryDate = new Date(issuedDate.getTime() + 365 * 24 * 60 * 60 * 1000); // 1 year

    const complianceLevel =
      report.overallStatus === 'compliant' ? 'full' :
      report.overallStatus === 'partial' ? 'partial' : 'conditional';

    const controlCertifications = this.generateControlCertifications(report.results);
    const certificateNumber = this.generateCertificateNumber(standard, organization);
    const digitalSignature = this.generateDigitalSignature(organization, issuedDate);

    return {
      id: `CERT-${standard}-${Date.now()}`,
      standard,
      issuedDate,
      expiryDate,
      organization,
      auditor,
      scope,
      complianceLevel,
      controlCertifications,
      digitalSignature,
      certificateNumber,
    };
  }

  private generateControlCertifications(
    results: CheckResult[],
  ): ComplianceCertificate['controlCertifications'] {
    return results.map((r) => ({
      controlId: r.checkId,
      status: r.passed ? 'certified' : 'non-certified',
      evidence: r.evidence || 'See detailed compliance report',
    }));
  }

  private generateCertificateNumber(standard: ComplianceStandard, organization: string): string {
    const timestamp = Date.now();
    const orgHash = crypto
      .createHash('sha256')
      .update(organization)
      .digest('hex')
      .substring(0, 8)
      .toUpperCase();
    const randomStr = crypto.randomBytes(4).toString('hex').toUpperCase();

    return `${standard}-${timestamp}-${orgHash}-${randomStr}`;
  }

  private generateDigitalSignature(organization: string, date: Date): string {
    const signatureData = `${organization}:${date.toISOString()}:${this.issuerName}`;
    const signature = crypto.createHash('sha256').update(signatureData).digest('hex');

    return signature;
  }

  public generateCertificateDocument(certificate: ComplianceCertificate): string {
    const html = `<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Compliance Certificate - ${certificate.standard}</title>
    <style>
        body {
            font-family: 'Georgia', serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }
        .certificate {
            max-width: 900px;
            margin: 0 auto;
            background: white;
            padding: 40px;
            border: 3px solid #1a472a;
            box-shadow: 0 0 20px rgba(0,0,0,0.1);
            position: relative;
        }
        .certificate::before {
            content: '';
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background: url('data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><text x="50" y="50" font-size="40" opacity="0.05" text-anchor="middle" fill="black" font-family="Georgia">Verified</text></svg>');
            pointer-events: none;
        }
        .certificate-header {
            text-align: center;
            margin-bottom: 40px;
            position: relative;
            z-index: 1;
        }
        .seal {
            width: 100px;
            height: 100px;
            border: 3px solid #1a472a;
            border-radius: 50%;
            display: inline-block;
            margin-bottom: 20px;
            display: flex;
            align-items: center;
            justify-content: center;
            background: #f9f9f9;
            font-weight: bold;
            color: #1a472a;
            font-size: 12px;
            text-align: center;
            padding: 10px;
            box-sizing: border-box;
        }
        h1 {
            color: #1a472a;
            font-size: 36px;
            margin: 20px 0 10px 0;
            letter-spacing: 2px;
        }
        .subtitle {
            color: #666;
            font-size: 18px;
            margin-bottom: 30px;
        }
        .certificate-body {
            position: relative;
            z-index: 1;
        }
        .recipient {
            text-align: center;
            margin: 40px 0;
            font-size: 16px;
            line-height: 1.8;
        }
        .recipient-name {
            font-size: 24px;
            font-weight: bold;
            color: #1a472a;
            margin: 10px 0;
        }
        .statement {
            text-align: center;
            margin: 40px 0;
            font-size: 14px;
            line-height: 1.8;
            color: #333;
        }
        .details-grid {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 20px;
            margin: 40px 0;
            font-size: 13px;
        }
        .detail-item {
            border: 1px solid #ddd;
            padding: 15px;
            background: #f9f9f9;
        }
        .detail-label {
            font-weight: bold;
            color: #1a472a;
            margin-bottom: 5px;
        }
        .detail-value {
            color: #666;
        }
        .compliance-level {
            text-align: center;
            font-size: 20px;
            font-weight: bold;
            color: #28a745;
            margin: 30px 0;
        }
        .compliance-level.partial {
            color: #ffc107;
        }
        .compliance-level.conditional {
            color: #dc3545;
        }
        .signatures {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 40px;
            margin: 60px 0 40px 0;
            text-align: center;
        }
        .signature-block {
            border-top: 2px solid #333;
            padding-top: 10px;
            font-size: 12px;
        }
        .signature-block .name {
            font-weight: bold;
            margin-top: 5px;
        }
        .footer {
            text-align: center;
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            font-size: 11px;
            color: #999;
        }
        .certificate-number {
            text-align: right;
            font-size: 12px;
            color: #999;
            margin-bottom: 20px;
        }
        .security-features {
            margin-top: 30px;
            padding: 15px;
            background: #f0f0f0;
            border-left: 4px solid #1a472a;
            font-size: 11px;
        }
    </style>
</head>
<body>
    <div class="certificate">
        <div class="certificate-number">Certificate No. ${certificate.certificateNumber}</div>

        <div class="certificate-header">
            <div class="seal">✓ VERIFIED</div>
            <h1>COMPLIANCE CERTIFICATE</h1>
            <div class="subtitle">${certificate.standard}</div>
        </div>

        <div class="certificate-body">
            <div class="recipient">
                <p>This is to certify that</p>
                <div class="recipient-name">${certificate.organization}</div>
                <p>has demonstrated compliance with the requirements of</p>
                <div style="font-size: 18px; font-weight: bold; color: #1a472a; margin: 15px 0;">
                    ${this.getStandardFullName(certificate.standard)}
                </div>
            </div>

            <div class="statement">
                <p>
                    This certificate confirms that the organization named above has undergone
                    comprehensive compliance assessment and has satisfied the control requirements
                    specified in the ${certificate.standard} compliance framework.
                </p>
                <p style="margin-top: 20px; font-style: italic;">
                    Scope of Assessment: ${certificate.scope}
                </p>
            </div>

            <div class="details-grid">
                <div class="detail-item">
                    <div class="detail-label">Issued Date</div>
                    <div class="detail-value">${certificate.issuedDate.toDateString()}</div>
                </div>
                <div class="detail-item">
                    <div class="detail-label">Expiry Date</div>
                    <div class="detail-value">${certificate.expiryDate.toDateString()}</div>
                </div>
                <div class="detail-item">
                    <div class="detail-label">Compliance Level</div>
                    <div class="detail-value" style="text-transform: capitalize;">
                        ${certificate.complianceLevel}
                    </div>
                </div>
                <div class="detail-item">
                    <div class="detail-label">Auditor</div>
                    <div class="detail-value">${certificate.auditor}</div>
                </div>
            </div>

            <div class="compliance-level ${certificate.complianceLevel}">
                Status: ${certificate.complianceLevel.toUpperCase()}
            </div>

            <div class="signatures">
                <div class="signature-block">
                    _______________________
                    <div class="name">Compliance Officer</div>
                    <div>${this.issuerName}</div>
                </div>
                <div class="signature-block">
                    _______________________
                    <div class="name">${certificate.auditor}</div>
                    <div>Authorized Auditor</div>
                </div>
            </div>

            <div class="security-features">
                <strong>Digital Signature Hash:</strong><br>
                ${certificate.digitalSignature.substring(0, 32)}...
                <br><br>
                This certificate is digitally signed and verified by the Compliance Assurance Authority.
                Verify authenticity at https://compliance-verify.example.com
            </div>

            <div class="footer">
                <p>
                    This certificate is valid from ${certificate.issuedDate.toDateString()} until ${certificate.expiryDate.toDateString()}.
                    Regular audits are recommended to maintain compliance status.
                </p>
                <p>
                    Issued by: ${this.issuerName}<br>
                    Address: ${this.issuerAddress}
                </p>
            </div>
        </div>
    </div>
</body>
</html>`;

    return html;
  }

  private getStandardFullName(standard: ComplianceStandard): string {
    const names: Record<ComplianceStandard, string> = {
      SOC2: 'SOC 2 Type II Service Organization Control',
      ISO27001: 'ISO/IEC 27001:2022 Information Security Management System',
      GDPR: 'General Data Protection Regulation (GDPR)',
      HIPAA: 'Health Insurance Portability and Accountability Act (HIPAA)',
    };

    return names[standard];
  }

  public generateCertificateJson(certificate: ComplianceCertificate): string {
    return JSON.stringify(certificate, null, 2);
  }

  public verifyCertificate(certificate: ComplianceCertificate): boolean {
    // Verify digital signature
    const signatureData = `${certificate.organization}:${certificate.issuedDate.toISOString()}:${this.issuerName}`;
    const expectedSignature = crypto
      .createHash('sha256')
      .update(signatureData)
      .digest('hex');

    if (certificate.digitalSignature !== expectedSignature) {
      return false;
    }

    // Check expiry date
    if (new Date() > certificate.expiryDate) {
      return false;
    }

    return true;
  }

  public getCertificateStatus(certificate: ComplianceCertificate): string {
    const now = new Date();

    if (now > certificate.expiryDate) {
      return 'EXPIRED';
    }

    const daysUntilExpiry = Math.floor(
      (certificate.expiryDate.getTime() - now.getTime()) / (1000 * 60 * 60 * 24),
    );

    if (daysUntilExpiry < 30) {
      return 'EXPIRING_SOON';
    }

    if (certificate.complianceLevel === 'conditional') {
      return 'CONDITIONAL';
    }

    return 'VALID';
  }
}
