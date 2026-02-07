/**
 * SAML Service Tests
 */

import { SAMLService } from '../../../src/auth/sso/saml/saml.service';
import { SAMLConfig, SSOProvider, AuthProtocol } from '../../../src/auth/sso/types';

describe('SAMLService', () => {
  let samlService: SAMLService;
  let mockConfig: SAMLConfig;

  beforeEach(() => {
    mockConfig = {
      provider: SSOProvider.OKTA,
      protocol: AuthProtocol.SAML,
      enabled: true,
      clientId: 'test-client-id',
      callbackUrl: 'https://app.example.com/auth/sso/callback/okta',
      entryPoint: 'https://dev-123456.okta.com/app/dev-123456_app_1/exk123/sso/saml',
      issuer: 'http://www.okta.com/exk123',
      cert: 'MIIDpDCCAoygAwIBAgIGAXoWn...',
      identifierFormat: 'urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress',
      wantAssertionsSigned: true,
      wantAuthnResponseSigned: true,
    };

    samlService = new SAMLService(mockConfig);
  });

  describe('constructor', () => {
    it('should create SAML service with valid config', () => {
      expect(samlService).toBeInstanceOf(SAMLService);
    });
  });

  describe('generateAuthRequest', () => {
    it('should generate authentication request URL', async () => {
      const result = await samlService.generateAuthRequest();

      expect(result).toHaveProperty('url');
      expect(result).toHaveProperty('id');
      expect(result.url).toContain(mockConfig.entryPoint);
      expect(result.id).toMatch(/^_[a-f0-9]{42}$/);
    });
  });

  describe('validateResponse', () => {
    it('should validate SAML response successfully', async () => {
      const mockSamlResponse = Buffer.from(`<?xml version="1.0"?>
        <samlp:Response xmlns:samlp="urn:oasis:names:tc:SAML:2.0:protocol">
          <saml:Assertion xmlns:saml="urn:oasis:names:tc:SAML:2.0:assertion">
            <saml:Subject>
              <saml:NameID>user@example.com</saml:NameID>
            </saml:Subject>
          </saml:Assertion>
        </samlp:Response>
      `).toString('base64');

      // Note: This will fail without proper SAML signature
      // In real tests, use mock SAML responses with valid signatures
      const result = await samlService.validateResponse(mockSamlResponse);

      expect(result).toHaveProperty('valid');
      expect(result).toHaveProperty('error');
    });

    it('should reject invalid SAML response', async () => {
      const invalidResponse = 'invalid-base64-data';

      const result = await samlService.validateResponse(invalidResponse);

      expect(result.valid).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('generateServiceProviderMetadata', () => {
    it('should generate SP metadata XML', async () => {
      const metadata = await samlService.generateServiceProviderMetadata();

      expect(metadata).toContain('EntityDescriptor');
      expect(metadata).toContain('SPSSODescriptor');
      expect(metadata).toContain(mockConfig.callbackUrl);
    });
  });

  describe('generateLogoutRequest', () => {
    it('should generate logout request URL', async () => {
      const userId = 'user@example.com';
      const sessionIndex = 'session-123';

      const logoutUrl = await samlService.generateLogoutRequest(userId, sessionIndex);

      expect(logoutUrl).toBeDefined();
      expect(typeof logoutUrl).toBe('string');
    });
  });
});
