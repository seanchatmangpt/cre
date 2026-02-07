# GCP Marketplace Deployer Container
FROM gcr.io/cloud-marketplace-tools/k8s/deployer_helm:0.11.8

# Install additional dependencies
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    curl \
    jq \
    && rm -rf /var/lib/apt/lists/*

# Install gcloud SDK
RUN curl https://sdk.cloud.google.com | bash
ENV PATH="/root/google-cloud-sdk/bin:${PATH}"

# Install kubectl (if not already present)
RUN curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl" \
    && install -o root -g root -m 0755 kubectl /usr/local/bin/kubectl \
    && rm kubectl

# Copy provisioning scripts
COPY scripts/ /scripts/
COPY manifests/ /data/
COPY config/ /data/config/
COPY schema.yaml /data/schema.yaml

# Make scripts executable
RUN chmod +x /scripts/*.sh

# Set working directory
WORKDIR /data

# The deployer will run the default entrypoint from the base image
# Custom provisioning happens via APPLICATION_MANIFEST and deploy scripts
ENTRYPOINT ["/scripts/deploy.sh"]
