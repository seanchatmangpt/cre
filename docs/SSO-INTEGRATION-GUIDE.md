# Enterprise SSO Integration Guide

Complete guide for integrating enterprise Single Sign-On with Google Workspace, Okta, and Azure AD.

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Architecture](#architecture)
- [Installation](#installation)
- [Configuration](#configuration)
- [Provider Setup](#provider-setup)
- [Usage](#usage)
- [API Reference](#api-reference)
- [Security Considerations](#security-considerations)
- [Troubleshooting](#troubleshooting)

## Overview

This enterprise SSO solution provides a unified authentication interface supporting:

- **SAML 2.0** - Enterprise-grade federated authentication
- **OAuth 2.0** - Industry-standard authorization framework
- **OpenID Connect (OIDC)** - Modern identity layer on OAuth 2.0

### Supported Providers

| Provider | SAML | OAuth2 | OIDC | Status |
|----------|------|--------|------|--------|
| Google Workspace | ❌ | ✅ | ✅ | Production Ready |
| Okta | ✅ | ❌ | ✅ | Production Ready |
| Azure AD / Entra ID | ✅ | ✅ | ✅ | Production Ready |

## Features

### Core Capabilities

- ✅ **Multi-Protocol Support** - SAML, OAuth2, OIDC
- ✅ **PKCE Support** - Enhanced security for OAuth2/OIDC flows
- ✅ **Token Management** - Automatic refresh and validation
- ✅ **Session Management** - Secure session handling with customizable stores
- ✅ **CSRF Protection** - State parameter validation
- ✅ **Group/Role Mapping** - Extract user groups and roles
- ✅ **Middleware Integration** - Express middleware for route protection
- ✅ **TypeScript** - Full type safety and IntelliSense

### Security Features

- JWT signature validation
- Certificate-based SAML validation
- Secure session cookies (HttpOnly, Secure, SameSite)
- State parameter for CSRF protection
- Token expiration handling
- Automatic token refresh

## Architecture

```
┌─────────────────┐
│   Application   │
└────────┬────────┘
         │
    ┌────▼─────┐
    │ Router   │ (Express Routes)
    └────┬─────┘
         │
    ┌────▼──────────┐
    │ SSO Controller│
    └────┬──────────┘
         │
    ┌────▼────────────┐
    │ Provider Registry│
    └────┬────────────┘
         │
    ┌────▼─────────────────────┐
    │  Provider Implementations │
    │  - Google Workspace      │
    │  - Okta                  │
    │  - Azure AD              │
    └────┬─────────────────────┘
         │
    ┌────▼──────────┐
    │ Protocol Layer│
    │ - SAML        │
    │ - OAuth2      │
    │ - OIDC        │
    └───────────────┘
```

## Installation

### 1. Install Dependencies

```bash
npm install @node-saml/node-saml axios jsonwebtoken jwks-rsa cookie-parser express
npm install --save-dev @types/cookie-parser @types/express @types/jsonwebtoken
```

### 2. Copy SSO Module

Copy the `/src/auth/sso` directory to your project.

### 3. Environment Configuration

Copy `/config/sso/.env.sso.example` to your project root as `.env` and configure:

```bash
cp config/sso/.env.sso.example .env
```

Edit `.env` with your provider credentials.

## Configuration

### Minimum Configuration

```env
# Session (Required)
SSO_SESSION_SECRET=your-secret-min-32-characters

# Enable at least one provider
SSO_GOOGLE_ENABLED=true
SSO_GOOGLE_CLIENT_ID=your-client-id
SSO_GOOGLE_CLIENT_SECRET=your-secret
SSO_GOOGLE_CALLBACK_URL=https://yourapp.com/auth/sso/callback/google-workspace
```

### Production Configuration

```env
# Security Settings
SSO_SESSION_SECURE=true
SSO_CSRF_PROTECTION=true
SSO_ALLOWED_ORIGINS=https://yourapp.com,https://www.yourapp.com

# Session Settings
SSO_SESSION_MAX_AGE=3600000
SSO_SESSION_COOKIE_NAME=sso_session
```

## Provider Setup

### Google Workspace

#### 1. Create OAuth 2.0 Credentials

1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Create or select a project
3. Navigate to **APIs & Services** > **Credentials**
4. Click **Create Credentials** > **OAuth 2.0 Client ID**
5. Select **Web application**
6. Add authorized redirect URIs:
   - `https://yourapp.com/auth/sso/callback/google-workspace`
7. Note the **Client ID** and **Client Secret**

#### 2. Configure Environment

```env
SSO_GOOGLE_ENABLED=true
SSO_GOOGLE_CLIENT_ID=123456789-xxx.apps.googleusercontent.com
SSO_GOOGLE_CLIENT_SECRET=GOCSPX-xxx
SSO_GOOGLE_CALLBACK_URL=https://yourapp.com/auth/sso/callback/google-workspace
SSO_GOOGLE_HOSTED_DOMAIN=yourcompany.com  # Optional: Restrict to domain
```

#### 3. Enable APIs

- Google+ API (for user info)
- Google Identity Services

### Okta

#### Option A: OIDC (Recommended)

1. **Create OIDC Application**
   - Okta Admin Console > **Applications** > **Create App Integration**
   - Select **OIDC - OpenID Connect**
   - Choose **Web Application**
   - Add redirect URI: `https://yourapp.com/auth/sso/callback/okta`
   - Save and note **Client ID** and **Client Secret**

2. **Configure Environment**
```env
SSO_OKTA_ENABLED=true
SSO_OKTA_PROTOCOL=oidc
SSO_OKTA_DOMAIN=dev-123456.okta.com
SSO_OKTA_CLIENT_ID=0oa1234567890
SSO_OKTA_CLIENT_SECRET=your-secret
SSO_OKTA_CALLBACK_URL=https://yourapp.com/auth/sso/callback/okta
SSO_OKTA_SCOPES=openid,email,profile,groups
```

#### Option B: SAML

1. **Create SAML Application**
   - Okta Admin Console > **Applications** > **Create App Integration**
   - Select **SAML 2.0**
   - Configure SAML settings:
     - Single sign-on URL: `https://yourapp.com/auth/sso/callback/okta`
     - Audience URI: `https://yourapp.com`
   - Download certificate

2. **Configure Environment**
```env
SSO_OKTA_ENABLED=true
SSO_OKTA_PROTOCOL=saml
SSO_OKTA_DOMAIN=dev-123456.okta.com
SSO_OKTA_CLIENT_ID=0oa1234567890
SSO_OKTA_ISSUER=http://www.okta.com/exk1234567890
SSO_OKTA_ENTRY_POINT=https://dev-123456.okta.com/app/.../sso/saml
SSO_OKTA_CERT=MIIDpDCCAoygAwIBAgIGAXoWn...
SSO_OKTA_CALLBACK_URL=https://yourapp.com/auth/sso/callback/okta
```

### Azure AD / Microsoft Entra ID

#### Option A: OIDC (Recommended)

1. **Register Application**
   - [Azure Portal](https://portal.azure.com/) > **Azure Active Directory** > **App registrations**
   - Click **New registration**
   - Name: Your App Name
   - Redirect URI: `https://yourapp.com/auth/sso/callback/azure-ad`
   - Register and note **Application (client) ID** and **Directory (tenant) ID**

2. **Create Client Secret**
   - Go to **Certificates & secrets** > **New client secret**
   - Copy the secret value immediately

3. **API Permissions**
   - Add permissions:
     - Microsoft Graph > **User.Read** (Delegated)
     - Microsoft Graph > **openid** (Delegated)
     - Microsoft Graph > **profile** (Delegated)
     - Microsoft Graph > **email** (Delegated)
   - Grant admin consent

4. **Configure Environment**
```env
SSO_AZURE_ENABLED=true
SSO_AZURE_PROTOCOL=oidc
SSO_AZURE_TENANT_ID=your-tenant-id
SSO_AZURE_CLIENT_ID=your-client-id
SSO_AZURE_CLIENT_SECRET=your-secret
SSO_AZURE_CALLBACK_URL=https://yourapp.com/auth/sso/callback/azure-ad
SSO_AZURE_SCOPES=openid,profile,email,User.Read
```

#### Option B: SAML

1. **Create Enterprise Application**
   - Azure Portal > **Azure Active Directory** > **Enterprise applications**
   - **New application** > **Create your own application**
   - Select **Integrate any other application you don't find in the gallery**

2. **Configure SAML**
   - Go to **Single sign-on** > **SAML**
   - Basic SAML Configuration:
     - Identifier (Entity ID): `https://yourapp.com`
     - Reply URL: `https://yourapp.com/auth/sso/callback/azure-ad`
   - Download Certificate (Base64)

3. **Configure Environment**
```env
SSO_AZURE_ENABLED=true
SSO_AZURE_PROTOCOL=saml
SSO_AZURE_TENANT_ID=your-tenant-id
SSO_AZURE_CLIENT_ID=your-client-id
SSO_AZURE_ISSUER=spn:your-app-id
SSO_AZURE_ENTRY_POINT=https://login.microsoftonline.com/your-tenant-id/saml2
SSO_AZURE_CERT=MIIDpDCCAoygAwIBAgIGAXoWn...
SSO_AZURE_CALLBACK_URL=https://yourapp.com/auth/sso/callback/azure-ad
```

## Usage

### Basic Integration

```typescript
import express from 'express';
import cookieParser from 'cookie-parser';
import { initializeSSO, requireSSOAuth } from './src/auth/sso';

const app = express();

app.use(express.json());
app.use(express.urlencoded({ extended: true }));
app.use(cookieParser());

// Initialize SSO
const { router, sessionManager, providerRegistry } = initializeSSO();

// Mount SSO routes
app.use('/auth/sso', router);

// Protected route example
app.get('/api/profile', requireSSOAuth({ sessionManager }), (req, res) => {
  res.json({
    user: req.ssoUser,
    session: req.ssoSession,
  });
});

app.listen(3000, () => {
  console.log('Server running on port 3000');
});
```

### Advanced Configuration

```typescript
import { initializeSSO } from './src/auth/sso';
import RedisStore from 'connect-redis'; // Production session store
import Redis from 'ioredis';

// Create Redis client
const redisClient = new Redis({
  host: 'localhost',
  port: 6379,
});

// Initialize with custom options
const sso = initializeSSO({
  sessionStore: new RedisStore({ client: redisClient }),
  sessionMaxAge: 7200000, // 2 hours
  sessionCookieName: 'auth_session',
  sessionSecure: true,
  csrfProtection: true,
});

app.use('/auth/sso', sso.router);
```

### Route Protection Middleware

```typescript
import {
  requireSSOAuth,
  optionalSSOAuth,
  requireRole,
  requireGroup,
  requireProvider,
} from './src/auth/sso';

// Required authentication
app.get('/api/private',
  requireSSOAuth({ sessionManager }),
  (req, res) => {
    res.json({ user: req.ssoUser });
  }
);

// Optional authentication
app.get('/api/public',
  optionalSSOAuth({ sessionManager }),
  (req, res) => {
    if (req.ssoUser) {
      res.json({ user: req.ssoUser, authenticated: true });
    } else {
      res.json({ authenticated: false });
    }
  }
);

// Role-based access
app.get('/api/admin',
  requireSSOAuth({ sessionManager }),
  requireRole(['admin', 'superuser']),
  (req, res) => {
    res.json({ message: 'Admin area' });
  }
);

// Group-based access
app.get('/api/team',
  requireSSOAuth({ sessionManager }),
  requireGroup('engineering'),
  (req, res) => {
    res.json({ message: 'Engineering team area' });
  }
);

// Provider-specific access
app.get('/api/google-only',
  requireSSOAuth({ sessionManager }),
  requireProvider('google-workspace'),
  (req, res) => {
    res.json({ message: 'Google Workspace users only' });
  }
);
```

## API Reference

### Endpoints

| Method | Path | Description |
|--------|------|-------------|
| GET | `/auth/sso/providers` | List available providers |
| GET | `/auth/sso/login/:provider` | Initiate SSO login |
| GET | `/auth/sso/callback/:provider` | OAuth2/OIDC callback |
| POST | `/auth/sso/callback/:provider` | SAML callback |
| POST | `/auth/sso/logout` | Logout user |
| GET | `/auth/sso/session` | Get current session |
| POST | `/auth/sso/refresh` | Refresh access token |

### Request Examples

#### Initiate Login

```bash
curl -X GET https://yourapp.com/auth/sso/login/google-workspace
# Redirects to Google login
```

#### Get Session

```bash
curl -X GET https://yourapp.com/auth/sso/session \
  --cookie "sso_session=session-id-here"
```

#### Logout

```bash
curl -X POST https://yourapp.com/auth/sso/logout \
  --cookie "sso_session=session-id-here"
```

## Security Considerations

### Production Checklist

- [ ] Use HTTPS in production (`SSO_SESSION_SECURE=true`)
- [ ] Generate strong session secret (min 32 characters)
- [ ] Enable CSRF protection (`SSO_CSRF_PROTECTION=true`)
- [ ] Configure `SSO_ALLOWED_ORIGINS` to specific domains
- [ ] Use production-grade session store (Redis, database)
- [ ] Implement rate limiting on auth endpoints
- [ ] Set up monitoring and alerting
- [ ] Regular security audits
- [ ] Rotate secrets regularly
- [ ] Use separate credentials for dev/staging/prod

### Best Practices

1. **Secret Management**
   - Never commit secrets to version control
   - Use environment variables or secret managers (AWS Secrets Manager, Azure Key Vault)
   - Rotate credentials regularly

2. **Session Security**
   - Use HttpOnly cookies to prevent XSS
   - Enable Secure flag in production
   - Set appropriate SameSite attribute
   - Implement session timeout
   - Consider sliding session expiration

3. **Token Handling**
   - Validate all tokens on every request
   - Handle token expiration gracefully
   - Implement automatic token refresh
   - Store tokens securely (never in localStorage)

4. **Error Handling**
   - Don't expose sensitive information in errors
   - Log security events
   - Implement proper error boundaries

## Troubleshooting

### Common Issues

#### "Invalid state" Error

**Cause**: CSRF token validation failed
**Solution**: Ensure cookies are enabled and state is preserved during redirect

#### "Token validation failed"

**Cause**: JWT signature verification failed or token expired
**Solution**:
- Check provider certificates are up-to-date
- Verify system clock is synchronized
- Ensure issuer URL matches exactly

#### "Session not found"

**Cause**: Session expired or cookies not being sent
**Solution**:
- Check session expiration settings
- Verify cookie domain and path settings
- Enable credentials in CORS if using cross-origin requests

#### SAML Response Validation Fails

**Cause**: Certificate mismatch or assertion not signed
**Solution**:
- Download latest certificate from IdP
- Verify wantAssertionsSigned settings
- Check clock skew (acceptedClockSkewMs)

### Debug Mode

Enable detailed logging:

```typescript
// Add to your initialization
process.env.NODE_ENV = 'development';
process.env.DEBUG = 'saml:*,oauth:*';
```

### Testing

Run the test suite:

```bash
npm test
npm run test:coverage
```

## Support

For issues and questions:
- Check the troubleshooting section
- Review provider-specific documentation
- Contact your IdP administrator for provider configuration issues

## License

MIT
