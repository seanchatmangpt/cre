output "cluster_endpoint" {
  description = "The endpoint for the Kubernetes cluster API server"
  value       = try(aws_eks_cluster.main.endpoint, "")
}

output "cluster_name" {
  description = "The name of the EKS cluster"
  value       = try(aws_eks_cluster.main.name, "")
}

output "cluster_version" {
  description = "The Kubernetes version for the cluster"
  value       = try(aws_eks_cluster.main.version, "")
}

output "cluster_arn" {
  description = "The Amazon Resource Name (ARN) of the cluster"
  value       = try(aws_eks_cluster.main.arn, "")
}

output "cluster_certificate_authority_data" {
  description = "Base64 encoded certificate data required to communicate with the cluster"
  value       = try(aws_eks_cluster.main.certificate_authority[0].data, "")
  sensitive   = true
}

output "cluster_security_group_id" {
  description = "Security group ID attached to the EKS cluster"
  value       = try(aws_eks_cluster.main.vpc_config[0].cluster_security_group_id, "")
}

output "node_group_id" {
  description = "EKS node group identifier"
  value       = try(aws_eks_node_group.main.id, "")
}

output "node_group_arn" {
  description = "Amazon Resource Name (ARN) of the EKS Node Group"
  value       = try(aws_eks_node_group.main.arn, "")
}

output "node_group_status" {
  description = "Status of the EKS Node Group. One of: CREATING, ACTIVE, UPDATING, DELETING, CREATE_FAILED, DELETE_FAILED, DEGRADED, UPDATE_FAILED"
  value       = try(aws_eks_node_group.main.status, "")
}

output "node_group_resources_autoscaling_groups" {
  description = "List of objects containing information about Auto Scaling Groups associated with the EKS Node Group"
  value       = try(aws_eks_node_group.main.resources[0].autoscaling_groups[*].name, [])
}

output "service_load_balancer_ip" {
  description = "External IP address of the LoadBalancer service"
  value       = try(data.kubernetes_service.load_balancer.status[0].load_balancer[0].ingress[0].hostname, "")
}

output "service_cluster_ip" {
  description = "Internal ClusterIP of the service"
  value       = try(data.kubernetes_service.main.spec[0].cluster_ip, "")
}

output "service_ports" {
  description = "Ports exposed by the service"
  value = try([
    for port in data.kubernetes_service.main.spec[0].port : {
      name       = port.name
      port       = port.port
      target_port = port.target_port
      protocol   = port.protocol
    }
  ], [])
}

output "kubeconfig" {
  description = "Kubernetes configuration to connect to the cluster"
  value = {
    cluster_endpoint = try(aws_eks_cluster.main.endpoint, "")
    cluster_ca       = try(aws_eks_cluster.main.certificate_authority[0].data, "")
    cluster_name     = try(aws_eks_cluster.main.name, "")
  }
  sensitive = true
}

output "configure_kubectl" {
  description = "Command to configure kubectl to use the EKS cluster"
  value       = "aws eks update-kubeconfig --region ${var.aws_region} --name ${try(aws_eks_cluster.main.name, '')}"
}

output "ingress_controller_endpoint" {
  description = "Ingress controller external endpoint"
  value       = try(data.kubernetes_service.ingress_controller.status[0].load_balancer[0].ingress[0].hostname, "")
}

output "ingress_controller_ip" {
  description = "Ingress controller external IP"
  value       = try(data.kubernetes_service.ingress_controller.status[0].load_balancer[0].ingress[0].ip, "")
}

output "rds_endpoint" {
  description = "RDS database endpoint"
  value       = try(aws_db_instance.main.endpoint, "")
  sensitive   = true
}

output "rds_database_name" {
  description = "RDS database name"
  value       = try(aws_db_instance.main.db_name, "")
}

output "rds_connection_string" {
  description = "RDS database connection string"
  value       = "postgresql://${try(aws_db_instance.main.username, "")}:${try(aws_db_instance.main.password, "")}@${try(aws_db_instance.main.endpoint, "")}/${try(aws_db_instance.main.db_name, "")}"
  sensitive   = true
}

output "elasticache_endpoint" {
  description = "ElastiCache cluster endpoint"
  value       = try(aws_elasticache_cluster.main.cache_nodes[0].address, "")
}

output "elasticache_port" {
  description = "ElastiCache cluster port"
  value       = try(aws_elasticache_cluster.main.cache_nodes[0].port, "")
}

output "elasticache_connection_string" {
  description = "ElastiCache connection string"
  value       = "${try(aws_elasticache_cluster.main.cache_nodes[0].address, "")}:${try(aws_elasticache_cluster.main.cache_nodes[0].port, "")}"
}

output "s3_bucket_name" {
  description = "S3 bucket name"
  value       = try(aws_s3_bucket.main.id, "")
}

output "s3_bucket_arn" {
  description = "S3 bucket ARN"
  value       = try(aws_s3_bucket.main.arn, "")
}

output "s3_bucket_region" {
  description = "S3 bucket region"
  value       = try(aws_s3_bucket.main.region, "")
}

output "vpc_id" {
  description = "VPC ID"
  value       = try(aws_vpc.main.id, "")
}

output "vpc_cidr" {
  description = "VPC CIDR block"
  value       = try(aws_vpc.main.cidr_block, "")
}

output "subnet_ids" {
  description = "List of subnet IDs"
  value       = try(aws_subnet.main[*].id, [])
}

output "private_subnet_ids" {
  description = "List of private subnet IDs"
  value       = try(aws_subnet.private[*].id, [])
}

output "nat_gateway_ids" {
  description = "List of NAT Gateway IDs"
  value       = try(aws_nat_gateway.main[*].id, [])
}

output "nat_gateway_ips" {
  description = "Elastic IPs of NAT Gateways"
  value       = try(aws_eip.nat[*].public_ip, [])
}

output "connection_info" {
  description = "Complete connection information for all services"
  value = {
    cluster = {
      endpoint = try(aws_eks_cluster.main.endpoint, "")
      name     = try(aws_eks_cluster.main.name, "")
      region   = var.aws_region
    }
    kubernetes = {
      api_server = try(aws_eks_cluster.main.endpoint, "")
      ca_data    = try(aws_eks_cluster.main.certificate_authority[0].data, "")
    }
    services = {
      load_balancer = try(data.kubernetes_service.load_balancer.status[0].load_balancer[0].ingress[0].hostname, "")
      cluster_ip    = try(data.kubernetes_service.main.spec[0].cluster_ip, "")
      ingress       = try(data.kubernetes_service.ingress_controller.status[0].load_balancer[0].ingress[0].hostname, "")
    }
    database = {
      endpoint          = try(aws_db_instance.main.endpoint, "")
      name              = try(aws_db_instance.main.db_name, "")
      port              = try(aws_db_instance.main.port, "")
      engine            = try(aws_db_instance.main.engine, "")
      version           = try(aws_db_instance.main.engine_version, "")
    }
    cache = {
      endpoint = try(aws_elasticache_cluster.main.cache_nodes[0].address, "")
      port     = try(aws_elasticache_cluster.main.cache_nodes[0].port, "")
      engine   = try(aws_elasticache_cluster.main.engine, "")
    }
    storage = {
      s3_bucket = try(aws_s3_bucket.main.id, "")
      s3_region = try(aws_s3_bucket.main.region, "")
      s3_arn    = try(aws_s3_bucket.main.arn, "")
    }
    network = {
      vpc_id            = try(aws_vpc.main.id, "")
      vpc_cidr          = try(aws_vpc.main.cidr_block, "")
      subnets           = try(aws_subnet.main[*].id, [])
      private_subnets  = try(aws_subnet.private[*].id, [])
      nat_gateway_ips  = try(aws_eip.nat[*].public_ip, [])
    }
  }
  sensitive = true
}

output "connectivity_check_commands" {
  description = "Commands to verify connectivity to cluster resources"
  value = {
    eks_cluster = "aws eks describe-cluster --name ${try(aws_eks_cluster.main.name, "")} --region ${var.aws_region}"
    kubectl_auth = "aws eks update-kubeconfig --region ${var.aws_region} --name ${try(aws_eks_cluster.main.name, "")}"
    kubectl_cluster_info = "kubectl cluster-info"
    kubectl_nodes = "kubectl get nodes"
    test_rds = "psql -h ${try(aws_db_instance.main.address, "")} -U ${try(aws_db_instance.main.username, "")} -d ${try(aws_db_instance.main.db_name, "")} -c 'SELECT 1'"
    test_redis = "redis-cli -h ${try(aws_elasticache_cluster.main.cache_nodes[0].address, "")} -p ${try(aws_elasticache_cluster.main.cache_nodes[0].port, "")} PING"
  }
}
