FROM gcr.io/cloud-marketplace-tools/k8s/deployer_helm:0.11.8

# Copy application manifest and schema
COPY manifest/application.yaml /data/manifest/
COPY manifest/schema.yaml /data/

# Copy RBAC manifests
COPY rbac/*.yaml /data/rbac/

# Copy Kubernetes manifests
COPY k8s/*.yaml /data/k8s/

# Copy billing configuration
COPY billing/*.yaml /data/billing/

# Copy Helm chart
COPY chart/ /data/chart/

# Copy deployment scripts
COPY deployer/deploy.sh /bin/deploy
RUN chmod +x /bin/deploy

# Set environment variables for enterprise features
ENV WAIT_FOR_READY_TIMEOUT=600
ENV TESTER_TIMEOUT=600

# Application metadata
LABEL marketplace.cloud.google.com/name="Enterprise Application Platform"
LABEL marketplace.cloud.google.com/version="1.0.0"
LABEL marketplace.cloud.google.com/partner="Your Company Name"
LABEL marketplace.cloud.google.com/partner_id="your-partner-id"
LABEL marketplace.cloud.google.com/product_id="enterprise-app-v1"

# Set entrypoint
ENTRYPOINT ["/bin/deploy_with_tests.sh"]
