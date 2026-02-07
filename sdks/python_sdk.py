"""
CRE Master API - Python Client SDK

Auto-generated Python SDK for CRE Master API v0.2.0

The Cuneiform runtime environment (CRE) is a distributed execution environment
for programming languages implemented in distributed Erlang.
"""

import requests
import json
from typing import Any, Dict, List, Optional
from dataclasses import dataclass, asdict
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


class CREMasterClient:
    """Client for CRE Master API"""

    def __init__(self, base_url: str = "http://localhost:4142", timeout: int = 30):
        """
        Initialize the CRE Master API client.

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

    def _request(self, method: str, path: str, params: Optional[Dict[str, Any]] = None,
                 json_data: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Make HTTP request to the API.

        Args:
            method: HTTP method (GET, POST, PUT, DELETE)
            path: API endpoint path
            params: Query parameters
            json_data: JSON request body

        Returns:
            dict: API response as dictionary

        Raises:
            BadRequest: If response status is 400
            NotFound: If response status is 404
            ServerError: If response status is 500+
            APIException: For other errors
        """
        url = urljoin(self.base_url, path)

        try:
            response = self.session.request(
                method,
                url,
                params=params,
                json=json_data,
                timeout=self.timeout
            )

            # Handle errors
            if response.status_code == 400:
                raise BadRequest(f"Bad Request: {response.text}")
            elif response.status_code == 404:
                raise NotFound(f"Not Found: {response.text}")
            elif response.status_code >= 500:
                raise ServerError(f"Server Error: {response.text}")

            response.raise_for_status()

            if response.text:
                return response.json()
            return {}

        except requests.exceptions.RequestException as e:
            raise APIException(f"Request failed: {str(e)}")

    def get_status(self) -> Dict[str, Any]:
        """
        Get CRE Master status

        Retrieves the current status of the CRE Master process, including:
        - Active worker list with status
        - Idle worker list
        - Pending applications queue
        - Subscriptions information

        Returns:
            dict: CRE status containing:
                - idle_workers (list): List of idle worker process identifiers
                - busy_workers (list): Workers currently executing applications
                - pending_queue (list): Applications waiting to be scheduled
                - subscriptions (list): Connected client processes
                - worker_count (int): Total registered workers
                - idle_count (int): Number of idle workers
                - busy_count (int): Number of busy workers
                - queue_length (int): Number of pending applications
                - timestamp (str): ISO 8601 timestamp

        Example:
            >>> client = CREMasterClient()
            >>> status = client.get_status()
            >>> print(f"Idle workers: {status['idle_count']}")
            >>> print(f"Busy workers: {status['busy_count']}")
        """
        return self._request("GET", "/status.json")

    def get_history(self) -> Dict[str, Any]:
        """
        Get execution history and cache

        Retrieves the execution history and cache map from the CRE Master.
        Contains all memoized application-result pairs for performance optimization.

        Returns:
            dict: CRE history containing:
                - cache_entries (int): Total number of cached entries
                - total_executions (int): Total number of executions
                - cache_hits (int): Number of cache hits
                - cache_misses (int): Number of cache misses
                - hit_rate (float): Cache efficiency (0.0 to 1.0)
                - cache_size_bytes (int): Total cache size in bytes
                - oldest_entry (dict): First cached entry with metadata
                - latest_entry (dict): Most recent cached entry
                - entries (list): Detailed list of cached entries

        Example:
            >>> client = CREMasterClient()
            >>> history = client.get_history()
            >>> print(f"Cache hit rate: {history['hit_rate'] * 100:.1f}%")
            >>> print(f"Cached entries: {history['cache_entries']}")
        """
        return self._request("GET", "/history.json")

    def close(self):
        """Close the session"""
        self.session.close()

    def __enter__(self):
        """Context manager entry"""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.close()


# Data Models

@dataclass
class WorkerStatus:
    """Status of a single worker"""
    worker_id: str
    status: str  # idle, busy
    current_app: Optional[str] = None
    start_time: Optional[str] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'WorkerStatus':
        """Create instance from dictionary"""
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


@dataclass
class ApplicationStatus:
    """Status of a pending application"""
    app_id: str
    queued_time: str
    priority: int = 0

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'ApplicationStatus':
        """Create instance from dictionary"""
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


@dataclass
class CacheEntry:
    """A single cached application-result pair"""
    app_id: str
    timestamp: str
    result_size_bytes: Optional[int] = None
    execution_time_ms: Optional[int] = None

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'CacheEntry':
        """Create instance from dictionary"""
        return cls(**{k: v for k, v in data.items() if k in cls.__dataclass_fields__})


# Convenience functions

def get_cre_status(base_url: str = "http://localhost:4142") -> Dict[str, Any]:
    """Convenience function to get CRE status"""
    with CREMasterClient(base_url) as client:
        return client.get_status()


def get_cre_history(base_url: str = "http://localhost:4142") -> Dict[str, Any]:
    """Convenience function to get CRE history"""
    with CREMasterClient(base_url) as client:
        return client.get_history()


if __name__ == "__main__":
    # Example usage
    try:
        with CREMasterClient() as client:
            print("Getting CRE status...")
            status = client.get_status()
            print(json.dumps(status, indent=2))

            print("\nGetting CRE history...")
            history = client.get_history()
            print(f"Cache entries: {history.get('cache_entries')}")
            print(f"Cache hit rate: {history.get('hit_rate', 0) * 100:.1f}%")

    except Exception as e:
        print(f"Error: {e}")
