#!/usr/bin/env python3
"""
CRE SDK Generator

Generates Python, Go, and Node.js client SDKs from OpenAPI specifications.
Supports CRE Master API and YAWL Dashboard API.

Usage:
    python3 generate-sdks.py [--output-dir ./sdks] [--api-spec ./openapi.yaml]
"""

import json
import yaml
import os
import sys
import argparse
from pathlib import Path
from typing import Dict, List, Any, Optional
from dataclasses import dataclass


@dataclass
class Endpoint:
    """API Endpoint representation"""
    path: str
    method: str
    operation_id: str
    summary: str
    description: str
    parameters: List[Dict[str, Any]]
    request_body: Optional[Dict[str, Any]]
    responses: Dict[str, Dict[str, Any]]


class OpenAPIParser:
    """Parse OpenAPI specification"""

    def __init__(self, spec_file: str):
        with open(spec_file, 'r') as f:
            self.spec = yaml.safe_load(f)

    def get_info(self) -> Dict[str, str]:
        """Extract API info"""
        return {
            'title': self.spec['info'].get('title', 'API'),
            'version': self.spec['info'].get('version', '1.0.0'),
            'description': self.spec['info'].get('description', ''),
        }

    def get_servers(self) -> List[Dict[str, str]]:
        """Extract server URLs"""
        return self.spec.get('servers', [{'url': 'http://localhost:4142'}])

    def get_endpoints(self) -> List[Endpoint]:
        """Extract all endpoints"""
        endpoints = []
        paths = self.spec.get('paths', {})

        for path, methods in paths.items():
            for method, details in methods.items():
                if method.lower() not in ['get', 'post', 'put', 'delete', 'patch']:
                    continue

                endpoints.append(Endpoint(
                    path=path,
                    method=method.upper(),
                    operation_id=details.get('operationId', method.lower()),
                    summary=details.get('summary', ''),
                    description=details.get('description', ''),
                    parameters=details.get('parameters', []),
                    request_body=details.get('requestBody'),
                    responses=details.get('responses', {}),
                ))

        return endpoints

    def get_schemas(self) -> Dict[str, Dict[str, Any]]:
        """Extract all schemas"""
        return self.spec.get('components', {}).get('schemas', {})


class SDKGenerator:
    """Base SDK generator"""

    def __init__(self, api_info: Dict[str, str], endpoints: List[Endpoint], schemas: Dict[str, Any]):
        self.api_info = api_info
        self.endpoints = endpoints
        self.schemas = schemas

    def generate(self) -> str:
        """Generate SDK code - override in subclasses"""
        raise NotImplementedError


class PythonSDKGenerator(SDKGenerator):
    """Generate Python SDK"""

    def generate(self) -> str:
        code = self._generate_header()
        code += self._generate_client_class()
        code += self._generate_methods()
        code += self._generate_models()
        return code

    def _generate_header(self) -> str:
        return f'''"""
{self.api_info['title']} - Python Client SDK

Auto-generated Python SDK for {self.api_info['title']} v{self.api_info['version']}

{self.api_info['description']}
"""

import requests
import json
from typing import Any, Dict, List, Optional
from dataclasses import dataclass, asdict
from enum import Enum
from urllib.parse import urljoin


class APIException(Exception):
    """Base exception for API errors"""
    pass


class BadRequest(APIException):
    """400 Bad Request error"""
    pass


class NotFound(APIException):
    """404 Not Found error"""
    pass


class ServerError(APIException):
    """500 Server error"""
    pass

'''

    def _generate_client_class(self) -> str:
        code = f'''
class {self._camel_case(self.api_info['title'].replace(' ', ''))}Client:
    """Client for {self.api_info['title']}"""

    def __init__(self, base_url: str = "http://localhost:4142", timeout: int = 30):
        """
        Initialize the API client.

        Args:
            base_url: Base URL for the API (default: http://localhost:4142)
            timeout: Request timeout in seconds (default: 30)
        """
        self.base_url = base_url.rstrip('/')
        self.timeout = timeout
        self.session = requests.Session()
        self.session.headers.update({
            'Content-Type': 'application/json',
            'Accept': 'application/json',
        })

    def _request(self, method: str, path: str, **kwargs) -> Dict[str, Any]:
        """Make HTTP request"""
        url = urljoin(self.base_url, path)

        try:
            response = self.session.request(
                method,
                url,
                timeout=self.timeout,
                **kwargs
            )

            # Handle errors
            if response.status_code == 400:
                raise BadRequest(f"Bad Request: {{response.text}}")
            elif response.status_code == 404:
                raise NotFound(f"Not Found: {{response.text}}")
            elif response.status_code >= 500:
                raise ServerError(f"Server Error: {{response.text}}")

            response.raise_for_status()

            if response.text:
                return response.json()
            return {{}}

        except requests.exceptions.RequestException as e:
            raise APIException(f"Request failed: {{str(e)}}")

'''
        return code

    def _generate_methods(self) -> str:
        code = ""

        for endpoint in self.endpoints:
            method_name = endpoint.operation_id
            http_method = endpoint.method.lower()
            path = endpoint.path

            # Extract path parameters
            path_params = [p['name'] for p in endpoint.parameters
                           if p.get('in') == 'path']
            query_params = [p['name'] for p in endpoint.parameters
                            if p.get('in') == 'query']

            # Generate method signature
            param_str = ", ".join([f"{p}: str" for p in path_params])
            if query_params:
                param_str += ", " if param_str else ""
                param_str += ", ".join([f"{p}: Optional[Any] = None" for p in query_params])

            if param_str:
                param_str = ", " + param_str

            code += f'''
    def {method_name}(self{param_str}) -> Dict[str, Any]:
        """
        {endpoint.summary}

        {endpoint.description or ''}

        Returns:
            dict: API response
        """
'''

            # Build path with parameters
            if path_params:
                path_expr = f'"{path}"'
                for param in path_params:
                    path_expr = path_expr.replace(f'{{{param}}}', f'{{{{param}}}}')\
                                          .replace(f'{{{param}}}', f'{{{param}}}')
                    # Actually format it properly
                path = path.replace('{' + param + '}', f'{{{param}}}')
                code += f'        path = f"{path}"\n'
            else:
                code += f'        path = "{path}"\n'

            # Add query parameters
            if query_params:
                code += '        params = {}\n'
                for param in query_params:
                    code += f'        if {param} is not None:\n'
                    code += f'            params["{param}"] = {param}\n'
                code += f'        return self._request("{http_method}", path, params=params)\n'
            else:
                code += f'        return self._request("{http_method}", path)\n'

        return code

    def _generate_models(self) -> str:
        code = "\n# Data Models\n\n"

        for schema_name, schema in self.schemas.items():
            if schema.get('type') == 'object':
                properties = schema.get('properties', {})
                code += f'@dataclass\nclass {schema_name}:\n'
                code += f'    """{schema.get("title", schema_name)}"""\n'

                # Generate properties
                if properties:
                    for prop_name, prop_schema in properties.items():
                        prop_type = self._python_type(prop_schema)
                        code += f'    {prop_name}: {prop_type}\n'
                else:
                    code += '    pass\n'

                code += '\n'

        return code

    def _python_type(self, schema: Dict[str, Any]) -> str:
        """Map JSON schema type to Python type"""
        schema_type = schema.get('type', 'Any')

        if schema_type == 'object':
            return 'Dict[str, Any]'
        elif schema_type == 'array':
            items = schema.get('items', {})
            item_type = self._python_type(items)
            return f'List[{item_type}]'
        elif schema_type == 'string':
            return 'str'
        elif schema_type == 'integer':
            return 'int'
        elif schema_type == 'number':
            return 'float'
        elif schema_type == 'boolean':
            return 'bool'
        else:
            return 'Any'

    def _camel_case(self, s: str) -> str:
        """Convert to CamelCase"""
        return ''.join(word.capitalize() for word in s.split('_'))


class GoSDKGenerator(SDKGenerator):
    """Generate Go SDK"""

    def generate(self) -> str:
        code = self._generate_header()
        code += self._generate_client_struct()
        code += self._generate_methods()
        code += self._generate_models()
        return code

    def _generate_header(self) -> str:
        return f'''// Package creapi provides a client SDK for {self.api_info['title']} v{self.api_info['version']}
//
// {self.api_info['description']}
package creapi

import (
\t"encoding/json"
\t"fmt"
\t"io"
\t"net/http"
\t"net/url"
\t"time"
)

// APIError represents an API error response
type APIError struct {
\tError   string                 `json:"error"`
\tMessage string                 `json:"message"`
\tDetails map[string]interface{} `json:"details,omitempty"`
\tTraceID string                 `json:"trace_id,omitempty"`
}

func (e *APIError) Error() string {
\treturn fmt.Sprintf("API Error (%s): %s", e.Error, e.Message)
}

'''

    def _generate_client_struct(self) -> str:
        return f'''
// Client is the main API client
type Client struct {{
\tBaseURL    string
\tTimeout    time.Duration
\tHTTPClient *http.Client
}}

// NewClient creates a new API client
func NewClient(baseURL string) *Client {{
\tif baseURL == "" {{
\t\tbaseURL = "http://localhost:4142"
\t}}
\treturn &Client{{
\t\tBaseURL: baseURL,
\t\tTimeout: 30 * time.Second,
\t\tHTTPClient: &http.Client{{
\t\t\tTimeout: 30 * time.Second,
\t\t}},
\t}}
}}

// request makes an HTTP request
func (c *Client) request(method, path string, params url.Values, body interface{{}}) ([]byte, error) {{
\turlStr := c.BaseURL + path
\tif len(params) > 0 {{
\t\turlStr += "?" + params.Encode()
\t}}

\treq, err := http.NewRequest(method, urlStr, nil)
\tif err != nil {{
\t\treturn nil, err
\t}}

\treq.Header.Set("Content-Type", "application/json")
\treq.Header.Set("Accept", "application/json")

\tresp, err := c.HTTPClient.Do(req)
\tif err != nil {{
\t\treturn nil, err
\t}}
\tdefer resp.Body.Close()

\trespBody, err := io.ReadAll(resp.Body)
\tif err != nil {{
\t\treturn nil, err
\t}}

\tif resp.StatusCode >= 400 {{
\t\tvar apiErr APIError
\t\tjson.Unmarshal(respBody, &apiErr)
\t\treturn nil, &apiErr
\t}}

\treturn respBody, nil
}}

'''

    def _generate_methods(self) -> str:
        code = ""

        for endpoint in self.endpoints:
            method_name = ''.join(word.capitalize() for word in endpoint.operation_id.split('_'))
            http_method = endpoint.method
            path = endpoint.path

            query_params = [p['name'] for p in endpoint.parameters
                            if p.get('in') == 'query']
            path_params = [p['name'] for p in endpoint.parameters
                           if p.get('in') == 'path']

            # Generate method signature
            params_sig = ""
            if path_params:
                params_sig = ", " + ", ".join([f"{p} string" for p in path_params])

            code += f'''
// {method_name} {endpoint.summary}
func (c *Client) {method_name}(ctx context.Context{params_sig}) (map[string]interface{{}}, error) {{
\tpath := "{path}"
'''

            if path_params:
                for param in path_params:
                    code += f'\tpath = strings.ReplaceAll(path, "{{{param}}}", {param})\n'

            code += '\tparams := url.Values{}\n'
            for param in query_params:
                code += f'\t// Add {param} parameter if needed\n'

            code += '\tresp, err := c.request(http.{}, path, params, nil)\n'.format(http_method)
            code += '\tif err != nil {\n\t\treturn nil, err\n\t}\n'
            code += '\tvar result map[string]interface{}\n'
            code += '\tjson.Unmarshal(resp, &result)\n'
            code += '\treturn result, nil\n'
            code += '}\n'

        return code

    def _generate_models(self) -> str:
        code = "\n// Data Models\n\n"

        for schema_name, schema in self.schemas.items():
            if schema.get('type') == 'object':
                code += f'// {schema_name} represents {schema.get("title", schema_name)}\n'
                code += f'type {schema_name} struct {{\n'

                properties = schema.get('properties', {})
                for prop_name, prop_schema in properties.items():
                    go_type = self._go_type(prop_schema)
                    json_tag = f'`json:"{prop_name}"`'
                    # Convert property name to PascalCase for Go
                    go_prop_name = ''.join(word.capitalize() for word in prop_name.split('_'))
                    code += f'\t{go_prop_name} {go_type} {json_tag}\n'

                code += '}\n\n'

        return code

    def _go_type(self, schema: Dict[str, Any]) -> str:
        """Map JSON schema type to Go type"""
        schema_type = schema.get('type', 'interface{}')

        if schema_type == 'object':
            return 'map[string]interface{}'
        elif schema_type == 'array':
            items = schema.get('items', {})
            item_type = self._go_type(items)
            return f'[]{item_type}'
        elif schema_type == 'string':
            return 'string'
        elif schema_type == 'integer':
            return 'int64'
        elif schema_type == 'number':
            return 'float64'
        elif schema_type == 'boolean':
            return 'bool'
        else:
            return 'interface{}'


class NodeJSSDKGenerator(SDKGenerator):
    """Generate Node.js SDK"""

    def generate(self) -> str:
        code = self._generate_header()
        code += self._generate_client_class()
        code += self._generate_methods()
        code += self._generate_export()
        return code

    def _generate_header(self) -> str:
        return f'''/**
 * {{@name {self.api_info['title']}Client}}
 * @description Node.js client SDK for {self.api_info['title']} v{self.api_info['version']}
 *
 * {{@example
 * const client = new CREClient("http://localhost:4142");
 * const status = await client.getStatus();
 * }}
 */

const fetch = require('node-fetch');

/**
 * Error class for API errors
 */
class APIError extends Error {{
  constructor(error, message, statusCode) {{
    super(message);
    this.error = error;
    this.statusCode = statusCode;
    this.name = 'APIError';
  }}
}}

/**
 * Main API Client
 */
class CREClient {{
  /**
   * Initialize the client
   * @param {{string}} baseURL - Base URL for the API
   * @param {{number}} timeout - Request timeout in ms
   */
  constructor(baseURL = 'http://localhost:4142', timeout = 30000) {{
    this.baseURL = baseURL.replace(/\/$/, '');
    this.timeout = timeout;
  }}

  /**
   * Make HTTP request
   * @private
   */
  async _request(method, path, options = {{}}) {{
    const url = new URL(path.replace(/^\//, ''), this.baseURL);

    if (options.params) {{
      Object.keys(options.params).forEach(key => {{
        if (options.params[key] !== undefined && options.params[key] !== null) {{
          url.searchParams.append(key, options.params[key]);
        }}
      }});
    }}

    const fetchOptions = {{
      method,
      headers: {{
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        ...options.headers,
      }},
      timeout: this.timeout,
    }};

    if (options.body) {{
      fetchOptions.body = JSON.stringify(options.body);
    }}

    try {{
      const response = await fetch(url.toString(), fetchOptions);
      const data = await response.json().catch(() => ({{}}));

      if (!response.ok) {{
        throw new APIError(data.error, data.message, response.status);
      }}

      return data;
    }} catch (error) {{
      if (error instanceof APIError) throw error;
      throw new APIError('REQUEST_ERROR', error.message, 0);
    }}
  }}

'''

    def _generate_client_class(self) -> str:
        return ""

    def _generate_methods(self) -> str:
        code = ""

        for endpoint in self.endpoints:
            method_name = endpoint.operation_id
            http_method = endpoint.method
            path = endpoint.path

            path_params = [p['name'] for p in endpoint.parameters
                           if p.get('in') == 'path']
            query_params = [p['name'] for p in endpoint.parameters
                            if p.get('in') == 'query']

            # Generate method signature
            params_sig = ""
            if path_params or query_params:
                params_list = path_params + query_params
                params_sig = f"options = {{}}"

            code += f'''
  /**
   * {endpoint.summary}
   * {endpoint.description or ''}
   */
  async {method_name}(options = {{}}) {{
'''

            # Build path
            if path_params:
                code += f'    let path = "{path}";\n'
                for param in path_params:
                    code += f'    if (options.{param}) {{\n'
                    code += f'      path = path.replace("{{{{{param}}}}}}", options.{param});\n'
                    code += f'    }}\n'
            else:
                code += f'    const path = "{path}";\n'

            code += f'    return this._request("{http_method}", path, {{\n'
            if query_params:
                code += '      params: {\n'
                for param in query_params:
                    code += f'        {param}: options.{param},\n'
                code += '      },\n'
            code += '    });\n'
            code += '  }\n'

        return code

    def _generate_export(self) -> str:
        return '''
module.exports = CREClient;
module.exports.APIError = APIError;
'''


def main():
    parser = argparse.ArgumentParser(
        description='Generate client SDKs from OpenAPI specifications'
    )
    parser.add_argument('--output-dir', default='./sdks',
                        help='Output directory for generated SDKs')
    parser.add_argument('--api-specs', nargs='+',
                        default=['./docs/openapi-cre-master.yaml',
                                 './docs/openapi-yawl-dashboard.yaml'],
                        help='OpenAPI specification files')

    args = parser.parse_args()

    # Create output directory
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    generators = {
        'python': (PythonSDKGenerator, '.py'),
        'go': (GoSDKGenerator, '.go'),
        'nodejs': (NodeJSSDKGenerator, '.js'),
    }

    # Generate SDKs for each spec file
    for spec_file in args.api_specs:
        spec_path = Path(spec_file)
        if not spec_path.exists():
            print(f"Warning: Spec file not found: {spec_file}")
            continue

        print(f"Processing {spec_file}...")

        # Parse spec
        parser = OpenAPIParser(spec_file)
        api_info = parser.get_info()
        endpoints = parser.get_endpoints()
        schemas = parser.get_schemas()

        # Generate SDKs
        for lang, (generator_class, ext) in generators.items():
            try:
                generator = generator_class(api_info, endpoints, schemas)
                code = generator.generate()

                # Get API name for filename
                api_name = api_info['title'].lower().replace(' ', '_')
                output_file = output_dir / f"{api_name}_client{ext}"

                with open(output_file, 'w') as f:
                    f.write(code)

                print(f"  Generated {lang} SDK: {output_file}")

            except Exception as e:
                print(f"  Error generating {lang} SDK: {e}")

    print(f"\nSDKs generated in {output_dir}/")


if __name__ == '__main__':
    main()
