// Azimuth 云原生集成测试用例
// 专注于云原生环境下的遥测系统集成和互操作性

// 测试1: Kubernetes环境下的遥测数据收集
test "Kubernetes环境下的遥测数据收集" {
  // Kubernetes Pod元数据
  type K8sPodMetadata = {
    name: String,
    namespace: String,
    uid: String,
    labels: Map[String, String],
    annotations: Map[String, String],
    node_name: String,
    pod_ip: String,
    service_account: String
  }
  
  // Kubernetes服务信息
  type K8sServiceInfo = {
    name: String,
    namespace: String,
    cluster_ip: String,
    external_ips: Array[String],
    ports: Array[(String, Int, String)>, // (name, port, protocol)
    selector: Map[String, String],
    type: String // ClusterIP, NodePort, LoadBalancer
  }
  
  // 遥测资源映射器
  let map_k8s_metadata_to_telemetry = fn(pod: K8sPodMetadata, service: Option[K8sServiceInfo>) {
    let mut attributes = [
      ("k8s.pod.name", pod.name),
      ("k8s.namespace.name", pod.namespace),
      ("k8s.node.name", pod.node_name),
      ("k8s.pod.ip", pod.pod_ip),
      ("k8s.pod.uid", pod.uid)
    ]
    
    // 添加标签
    for (key, value) in pod.labels {
      attributes = attributes.push(("k8s.pod.label." + key, value))
    }
    
    // 添加注解
    for (key, value) in pod.annotations {
      attributes = attributes.push(("k8s.pod.annotation." + key, value))
    }
    
    // 如果有服务信息，添加服务相关属性
    match service {
      Some(svc) => {
        attributes = attributes.push([
          ("k8s.service.name", svc.name),
          ("k8s.service.namespace", svc.namespace),
          ("k8s.service.cluster_ip", svc.cluster_ip),
          ("k8s.service.type", svc.type)
        ])
        
        // 添加服务选择器
        for (key, value) in svc.selector {
          attributes = attributes.push(("k8s.service.selector." + key, value))
        }
      }
      None => {}
    }
    
    attributes
  }
  
  // 创建测试Pod元数据
  let test_pod = {
    name: "payment-service-7d4f8c9b5-k8s2x",
    namespace: "production",
    uid: "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
    labels: [
      ("app", "payment-service"),
      ("version", "v2.3.1"),
      ("tier", "backend"),
      ("environment", "production")
    ],
    annotations: [
      ("prometheus.io/scrape", "true"),
      ("prometheus.io/port", "8080"),
      ("prometheus.io/path", "/metrics")
    ],
    node_name: "worker-node-3",
    pod_ip: "10.244.2.15",
    service_account: "payment-service-sa"
  }
  
  // 创建测试服务信息
  let test_service = {
    name: "payment-service",
    namespace: "production",
    cluster_ip: "10.96.123.45",
    external_ips: [],
    ports: [
      ("http", 8080, "TCP"),
      ("grpc", 9090, "TCP")
    ],
    selector: [
      ("app", "payment-service")
    ],
    type: "ClusterIP"
  }
  
  // 测试元数据映射
  let telemetry_attributes = map_k8s_metadata_to_telemetry(test_pod, Some(test_service))
  
  // 验证基本属性
  assert_true(telemetry_attributes.contains(("k8s.pod.name", "payment-service-7d4f8c9b5-k8s2x")))
  assert_true(telemetry_attributes.contains(("k8s.namespace.name", "production")))
  assert_true(telemetry_attributes.contains(("k8s.node.name", "worker-node-3")))
  assert_true(telemetry_attributes.contains(("k8s.pod.ip", "10.244.2.15")))
  
  // 验证标签映射
  assert_true(telemetry_attributes.contains(("k8s.pod.label.app", "payment-service")))
  assert_true(telemetry_attributes.contains(("k8s.pod.label.version", "v2.3.1")))
  assert_true(telemetry_attributes.contains(("k8s.pod.label.tier", "backend")))
  assert_true(telemetry_attributes.contains(("k8s.pod.label.environment", "production")))
  
  // 验证注解映射
  assert_true(telemetry_attributes.contains(("k8s.pod.annotation.prometheus.io/scrape", "true")))
  assert_true(telemetry_attributes.contains(("k8s.pod.annotation.prometheus.io/port", "8080")))
  assert_true(telemetry_attributes.contains(("k8s.pod.annotation.prometheus.io/path", "/metrics")))
  
  // 验证服务属性
  assert_true(telemetry_attributes.contains(("k8s.service.name", "payment-service")))
  assert_true(telemetry_attributes.contains(("k8s.service.namespace", "production")))
  assert_true(telemetry_attributes.contains(("k8s.service.cluster_ip", "10.96.123.45")))
  assert_true(telemetry_attributes.contains(("k8s.service.type", "ClusterIP")))
  
  // 测试无服务信息的情况
  let attributes_without_service = map_k8s_metadata_to_telemetry(test_pod, None)
  
  // 应该包含Pod属性但不包含服务属性
  assert_true(attributes_without_service.contains(("k8s.pod.name", "payment-service-7d4f8c9b5-k8s2x")))
  assert_false(attributes_without_service.contains(("k8s.service.name", "payment-service")))
}

// 测试2: 服务网格遥测集成
test "服务网格遥测集成" {
  // 服务网格元数据
  type ServiceMeshMetadata = {
    mesh_id: String,
    mesh_name: String,
    workload_name: String,
    workload_namespace: String,
    workload_kind: String, // Deployment, StatefulSet, etc.
    service_name: String,
    service_namespace: String,
    destination_name: String,
    destination_namespace: String,
    destination_service: String,
    destination_workload: String,
    request_protocol: String,
    response_flags: Array[String],
    connection_security_policy: String
  }
  
  // Istio特定的遥测属性
  let extract_istio_attributes = fn(metadata: ServiceMeshMetadata) {
    let mut attributes = [
      ("service_mesh.id", metadata.mesh_id),
      ("service_mesh.name", metadata.mesh_name),
      ("source.workload.name", metadata.workload_name),
      ("source.workload.namespace", metadata.workload_namespace),
      ("source.workload.kind", metadata.workload_kind),
      ("source.service.name", metadata.service_name),
      ("source.service.namespace", metadata.service_namespace),
      ("destination.service.name", metadata.destination_service),
      ("destination.workload.name", metadata.destination_workload),
      ("destination.service.namespace", metadata.destination_namespace),
      ("destination.workload.namespace", metadata.destination_namespace),
      ("request.protocol", metadata.request_protocol),
      ("connection.security.policy", metadata.connection_security_policy)
    ]
    
    // 添加响应标志
    for flag in metadata.response_flags {
      attributes = attributes.push(("response.flag", flag))
    }
    
    attributes
  }
  
  // 创建服务网格测试数据
  let mesh_metadata = {
    mesh_id: "cluster-1",
    mesh_name: "istio",
    workload_name: "productpage-v1",
    workload_namespace: "default",
    workload_kind: "Deployment",
    service_name: "productpage",
    service_namespace: "default",
    destination_name: "reviews",
    destination_namespace: "default",
    destination_service: "reviews.default.svc.cluster.local",
    destination_workload: "reviews-v1",
    request_protocol: "http",
    response_flags: ["NR"], // No route found
    connection_security_policy: "mutual_tls"
  }
  
  // 提取Istio属性
  let istio_attributes = extract_istio_attributes(mesh_metadata)
  
  // 验证基本属性
  assert_true(istio_attributes.contains(("service_mesh.id", "cluster-1")))
  assert_true(istio_attributes.contains(("service_mesh.name", "istio")))
  assert_true(istio_attributes.contains(("source.workload.name", "productpage-v1")))
  assert_true(istio_attributes.contains(("source.workload.namespace", "default")))
  assert_true(istio_attributes.contains(("source.workload.kind", "Deployment")))
  
  // 验证目标属性
  assert_true(istio_attributes.contains(("destination.service.name", "reviews.default.svc.cluster.local")))
  assert_true(istio_attributes.contains(("destination.workload.name", "reviews-v1")))
  assert_true(istio_attributes.contains(("destination.service.namespace", "default")))
  
  // 验证协议和安全属性
  assert_true(istio_attributes.contains(("request.protocol", "http")))
  assert_true(istio_attributes.contains(("connection.security.policy", "mutual_tls")))
  
  // 验证响应标志
  assert_true(istio_attributes.contains(("response.flag", "NR")))
}

// 测试3: 云服务提供商集成
test "云服务提供商集成" {
  // 云提供商元数据
  type CloudProviderMetadata = {
    provider: String, // aws, azure, gcp, etc.
    region: String,
    zone: String,
    account_id: String,
    project_id: String,
    instance_id: String,
    instance_type: String,
    instance_name: String,
    private_ip: String,
    public_ip: String,
    vpc_id: String,
    subnet_id: String,
    security_groups: Array[String],
    tags: Map[String, String]
  }
  
  // AWS特定的遥测属性
  let extract_aws_attributes = fn(metadata: CloudProviderMetadata) {
    let mut attributes = [
      ("cloud.provider", "aws"),
      ("cloud.region", metadata.region),
      ("cloud.zone", metadata.zone),
      ("cloud.account.id", metadata.account_id),
      ("aws.instance.id", metadata.instance_id),
      ("aws.instance.type", metadata.instance_type),
      ("aws.instance.name", metadata.instance_name),
      ("aws.private.ip", metadata.private_ip),
      ("aws.public.ip", metadata.public_ip),
      ("aws.vpc.id", metadata.vpc_id),
      ("aws.subnet.id", metadata.subnet_id)
    ]
    
    // 添加安全组
    for sg in metadata.security_groups {
      attributes = attributes.push(("aws.security.group", sg))
    }
    
    // 添加标签
    for (key, value) in metadata.tags {
      attributes = attributes.push(("aws.tag." + key, value))
    }
    
    attributes
  }
  
  // Azure特定的遥测属性
  let extract_azure_attributes = fn(metadata: CloudProviderMetadata) {
    let mut attributes = [
      ("cloud.provider", "azure"),
      ("cloud.region", metadata.region),
      ("cloud.zone", metadata.zone),
      ("azure.subscription.id", metadata.account_id),
      ("azure.project.id", metadata.project_id),
      ("azure.vm.id", metadata.instance_id),
      ("azure.vm.size", metadata.instance_type),
      ("azure.vm.name", metadata.instance_name),
      ("azure.private.ip", metadata.private_ip),
      ("azure.public.ip", metadata.public_ip),
      ("azure.vnet.id", metadata.vpc_id),
      ("azure.subnet.id", metadata.subnet_id)
    ]
    
    // 添加标签
    for (key, value) in metadata.tags {
      attributes = attributes.push(("azure.tag." + key, value))
    }
    
    attributes
  }
  
  // GCP特定的遥测属性
  let extract_gcp_attributes = fn(metadata: CloudProviderMetadata) {
    let mut attributes = [
      ("cloud.provider", "gcp"),
      ("cloud.region", metadata.region),
      ("cloud.zone", metadata.zone),
      ("gcp.project.id", metadata.project_id),
      ("gcp.instance.id", metadata.instance_id),
      ("gcp.instance.type", metadata.instance_type),
      ("gcp.instance.name", metadata.instance_name),
      ("gcp.private.ip", metadata.private_ip),
      ("gcp.public.ip", metadata.public_ip),
      ("gcp.network.id", metadata.vpc_id),
      ("gcp.subnetwork.id", metadata.subnet_id)
    ]
    
    // 添加标签
    for (key, value) in metadata.tags {
      attributes = attributes.push(("gcp.label." + key, value))
    }
    
    attributes
  }
  
  // 创建AWS测试数据
  let aws_metadata = {
    provider: "aws",
    region: "us-west-2",
    zone: "us-west-2a",
    account_id: "123456789012",
    project_id: "",
    instance_id: "i-0123456789abcdef0",
    instance_type: "t3.medium",
    instance_name: "web-server-1",
    private_ip: "10.0.1.100",
    public_ip: "54.212.123.45",
    vpc_id: "vpc-0123456789abcdef0",
    subnet_id: "subnet-0123456789abcdef0",
    security_groups: ["sg-0123456789abcdef0", "sg-fedcba9876543210f"],
    tags: [
      ("Environment", "production"),
      ("Application", "web-service"),
      ("Owner", "platform-team"),
      ("CostCenter", "engineering")
    ]
  }
  
  // 提取AWS属性
  let aws_attributes = extract_aws_attributes(aws_metadata)
  
  // 验证基本AWS属性
  assert_true(aws_attributes.contains(("cloud.provider", "aws")))
  assert_true(aws_attributes.contains(("cloud.region", "us-west-2")))
  assert_true(aws_attributes.contains(("cloud.zone", "us-west-2a")))
  assert_true(aws_attributes.contains(("cloud.account.id", "123456789012")))
  assert_true(aws_attributes.contains(("aws.instance.id", "i-0123456789abcdef0")))
  assert_true(aws_attributes.contains(("aws.instance.type", "t3.medium")))
  assert_true(aws_attributes.contains(("aws.instance.name", "web-server-1")))
  
  // 验证网络属性
  assert_true(aws_attributes.contains(("aws.private.ip", "10.0.1.100")))
  assert_true(aws_attributes.contains(("aws.public.ip", "54.212.123.45")))
  assert_true(aws_attributes.contains(("aws.vpc.id", "vpc-0123456789abcdef0")))
  assert_true(aws_attributes.contains(("aws.subnet.id", "subnet-0123456789abcdef0")))
  
  // 验证安全组
  assert_true(aws_attributes.contains(("aws.security.group", "sg-0123456789abcdef0")))
  assert_true(aws_attributes.contains(("aws.security.group", "sg-fedcba9876543210f")))
  
  // 验证标签
  assert_true(aws_attributes.contains(("aws.tag.Environment", "production")))
  assert_true(aws_attributes.contains(("aws.tag.Application", "web-service")))
  assert_true(aws_attributes.contains(("aws.tag.Owner", "platform-team")))
  assert_true(aws_attributes.contains(("aws.tag.CostCenter", "engineering")))
  
  // 创建GCP测试数据
  let gcp_metadata = {
    provider: "gcp",
    region: "us-central1",
    zone: "us-central1-a",
    account_id: "",
    project_id: "my-gcp-project-12345",
    instance_id: "1234567890123456789",
    instance_type: "n1-standard-1",
    instance_name: "web-server-1",
    private_ip: "10.128.0.100",
    public_ip: "34.123.45.67",
    vpc_id: "my-vpc-network",
    subnet_id: "my-subnet-1",
    security_groups: [],
    tags: [
      ("environment", "production"),
      ("application", "web-service"),
      ("team", "platform")
    ]
  }
  
  // 提取GCP属性
  let gcp_attributes = extract_gcp_attributes(gcp_metadata)
  
  // 验证基本GCP属性
  assert_true(gcp_attributes.contains(("cloud.provider", "gcp")))
  assert_true(gcp_attributes.contains(("cloud.region", "us-central1")))
  assert_true(gcp_attributes.contains(("cloud.zone", "us-central1-a")))
  assert_true(gcp_attributes.contains(("gcp.project.id", "my-gcp-project-12345")))
  assert_true(gcp_attributes.contains(("gcp.instance.id", "1234567890123456789")))
  assert_true(gcp_attributes.contains(("gcp.instance.type", "n1-standard-1")))
  
  // 验证GCP标签
  assert_true(gcp_attributes.contains(("gcp.label.environment", "production")))
  assert_true(gcp_attributes.contains(("gcp.label.application", "web-service")))
  assert_true(gcp_attributes.contains(("gcp.label.team", "platform")))
}

// 测试4: 无服务器函数遥测集成
test "无服务器函数遥测集成" {
  // 无服务器函数元数据
  type ServerlessFunctionMetadata = {
    provider: String, // aws, azure, gcp
    region: String,
    function_name: String,
    function_version: String,
    function_arn: String,
    function_memory_size: Int,
    function_timeout: Int,
    runtime: String,
    handler: String,
    invocation_id: String,
    request_id: String,
    x_amzn_trace_id: String,
    remaining_execution_time: Int
  }
  
  // AWS Lambda特定的遥测属性
  let extract_lambda_attributes = fn(metadata: ServerlessFunctionMetadata) {
    [
      ("faas.provider", "aws"),
      ("faas.name", metadata.function_name),
      ("faas.version", metadata.function_version),
      ("faas.id", metadata.function_arn),
      ("faas.memory", metadata.function_memory_size.to_string()),
      ("faas.max_memory", metadata.function_memory_size.to_string()),
      ("faas.execution", metadata.invocation_id),
      ("faas.invocation_id", metadata.invocation_id),
      ("aws.lambda.invoked_arn", metadata.function_arn),
      ("aws.lambda.request_id", metadata.request_id),
      ("aws.lambda.runtime", metadata.runtime),
      ("aws.lambda.handler", metadata.handler)
    ]
  }
  
  // 创建Lambda测试数据
  let lambda_metadata = {
    provider: "aws",
    region: "us-east-1",
    function_name: "process-payment",
    function_version: "$LATEST",
    function_arn: "arn:aws:lambda:us-east-1:123456789012:function:process-payment",
    function_memory_size: 512,
    function_timeout: 30,
    runtime: "python3.9",
    handler: "lambda_function.lambda_handler",
    invocation_id: "5d828b65-f8c3-4f7c-9b4a-123456789012",
    request_id: "6b7a9d0e-1f2c-3d4e-5f6a-123456789012",
    x_amzn_trace_id: "Root=1-5e6722a7-1234567890abcdef123456",
    remaining_execution_time: 25
  }
  
  // 提取Lambda属性
  let lambda_attributes = extract_lambda_attributes(lambda_metadata)
  
  // 验证Lambda属性
  assert_true(lambda_attributes.contains(("faas.provider", "aws")))
  assert_true(lambda_attributes.contains(("faas.name", "process-payment")))
  assert_true(lambda_attributes.contains(("faas.version", "$LATEST")))
  assert_true(lambda_attributes.contains(("faas.id", "arn:aws:lambda:us-east-1:123456789012:function:process-payment")))
  assert_true(lambda_attributes.contains(("faas.memory", "512")))
  assert_true(lambda_attributes.contains(("faas.max_memory", "512")))
  assert_true(lambda_attributes.contains(("faas.execution", "5d828b65-f8c3-4f7c-9b4a-123456789012")))
  assert_true(lambda_attributes.contains(("faas.invocation_id", "5d828b65-f8c3-4f7c-9b4a-123456789012")))
  assert_true(lambda_attributes.contains(("aws.lambda.invoked_arn", "arn:aws:lambda:us-east-1:123456789012:function:process-payment")))
  assert_true(lambda_attributes.contains(("aws.lambda.request_id", "6b7a9d0e-1f2c-3d4e-5f6a-123456789012")))
  assert_true(lambda_attributes.contains(("aws.lambda.runtime", "python3.9")))
  assert_true(lambda_attributes.contains(("aws.lambda.handler", "lambda_function.lambda_handler")))
}

// 测试5: 容器运行时遥测集成
test "容器运行时遥测集成" {
  // 容器元数据
  type ContainerMetadata = {
    container_id: String,
    container_name: String,
    image_name: String,
    image_id: String,
    image_tag: String,
    pod_name: String,
    pod_namespace: String,
    pod_uid: String,
    container_labels: Map[String, String],
    container_env: Array[String>,
    container_created: Int,
    container_started: Int,
    container_exit_code: Option[Int>
  }
  
  // 容器运行时属性提取器
  let extract_container_attributes = fn(metadata: ContainerMetadata) {
    let mut attributes = [
      ("container.id", metadata.container_id),
      ("container.name", metadata.container_name),
      ("container.image.name", metadata.image_name),
      ("container.image.id", metadata.image_id),
      ("container.image.tag", metadata.image_tag),
      ("k8s.pod.name", metadata.pod_name),
      ("k8s.namespace.name", metadata.pod_namespace),
      ("k8s.pod.uid", metadata.pod_uid),
      ("container.created", metadata.container_created.to_string()),
      ("container.started", metadata.container_started.to_string())
    ]
    
    // 添加容器标签
    for (key, value) in metadata.container_labels {
      attributes = attributes.push(("container.label." + key, value))
    }
    
    // 添加容器退出码（如果存在）
    match metadata.container_exit_code {
      Some(code) => {
        attributes = attributes.push(("container.exit.code", code.to_string()))
      }
      None => {}
    }
    
    attributes
  }
  
  // 创建测试容器元数据
  let container_metadata = {
    container_id: "a1b2c3d4e5f6g7h8i9j0",
    container_name: "payment-service",
    image_name: "my-registry/payment-service",
    image_id: "sha256:1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef",
    image_tag: "v2.3.1",
    pod_name: "payment-service-7d4f8c9b5-k8s2x",
    pod_namespace: "production",
    pod_uid: "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
    container_labels: [
      ("app", "payment-service"),
      ("version", "v2.3.1"),
      ("maintainer", "platform-team")
    ],
    container_env: [
      "ENVIRONMENT=production",
      "LOG_LEVEL=info",
      "DATABASE_URL=postgresql://db:5432/payments"
    ],
    container_created: 1625097600,
    container_started: 1625097610,
    container_exit_code: None
  }
  
  // 提取容器属性
  let container_attributes = extract_container_attributes(container_metadata)
  
  // 验证基本容器属性
  assert_true(container_attributes.contains(("container.id", "a1b2c3d4e5f6g7h8i9j0")))
  assert_true(container_attributes.contains(("container.name", "payment-service")))
  assert_true(container_attributes.contains(("container.image.name", "my-registry/payment-service")))
  assert_true(container_attributes.contains(("container.image.id", "sha256:1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef")))
  assert_true(container_attributes.contains(("container.image.tag", "v2.3.1")))
  
  // 验证Pod属性
  assert_true(container_attributes.contains(("k8s.pod.name", "payment-service-7d4f8c9b5-k8s2x")))
  assert_true(container_attributes.contains(("k8s.namespace.name", "production")))
  assert_true(container_attributes.contains(("k8s.pod.uid", "a1b2c3d4-e5f6-7890-abcd-ef1234567890")))
  
  // 验证时间戳
  assert_true(container_attributes.contains(("container.created", "1625097600")))
  assert_true(container_attributes.contains(("container.started", "1625097610")))
  
  // 验证容器标签
  assert_true(container_attributes.contains(("container.label.app", "payment-service")))
  assert_true(container_attributes.contains(("container.label.version", "v2.3.1")))
  assert_true(container_attributes.contains(("container.label.maintainer", "platform-team")))
  
  // 测试已退出的容器
  let exited_container = { ...container_metadata, container_exit_code: Some(0) }
  let exited_attributes = extract_container_attributes(exited_container)
  assert_true(exited_attributes.contains(("container.exit.code", "0")))
}

// 测试6: 云原生配置管理集成
test "云原生配置管理集成" {
  // 配置元数据
  type ConfigMetadata = {
    config_map_name: String,
    config_map_namespace: String,
    config_map_uid: String,
    secret_name: String,
    secret_namespace: String,
    secret_uid: String,
    config_data: Map[String, String],
    secret_data: Map[String, String],
    config_mount_path: String,
    secret_mount_path: String
  }
  
  // 配置属性提取器
  let extract_config_attributes = fn(metadata: ConfigMetadata) {
    let mut attributes = [
      ("k8s.configmap.name", metadata.config_map_name),
      ("k8s.configmap.namespace", metadata.config_map_namespace),
      ("k8s.configmap.uid", metadata.config_map_uid),
      ("k8s.secret.name", metadata.secret_name),
      ("k8s.secret.namespace", metadata.secret_namespace),
      ("k8s.secret.uid", metadata.secret_uid),
      ("k8s.configmap.mount.path", metadata.config_mount_path),
      ("k8s.secret.mount.path", metadata.secret_mount_path)
    ]
    
    // 添加配置数据键
    for key in metadata.config_data.keys() {
      attributes = attributes.push(("k8s.configmap.key", key))
    }
    
    // 添加密钥数据键
    for key in metadata.secret_data.keys() {
      attributes = attributes.push(("k8s.secret.key", key))
    }
    
    attributes
  }
  
  // 创建测试配置元数据
  let config_metadata = {
    config_map_name: "payment-service-config",
    config_map_namespace: "production",
    config_map_uid: "b2c3d4e5-f6g7-8901-bcde-f12345678901",
    secret_name: "payment-service-secrets",
    secret_namespace: "production",
    secret_uid: "c3d4e5f6-g7h8-9012-cdef-234567890123",
    config_data: [
      ("database.host", "db.production.svc.cluster.local"),
      ("database.port", "5432"),
      ("cache.ttl", "300"),
      ("log.level", "info")
    ],
    secret_data: [
      ("database.username", "payment_user"),
      ("database.password", "secret_password"),
      ("api.key", "api_key_value")
    ],
    config_mount_path: "/etc/config",
    secret_mount_path: "/etc/secrets"
  }
  
  // 提取配置属性
  let config_attributes = extract_config_attributes(config_metadata)
  
  // 验证ConfigMap属性
  assert_true(config_attributes.contains(("k8s.configmap.name", "payment-service-config")))
  assert_true(config_attributes.contains(("k8s.configmap.namespace", "production")))
  assert_true(config_attributes.contains(("k8s.configmap.uid", "b2c3d4e5-f6g7-8901-bcde-f12345678901")))
  assert_true(config_attributes.contains(("k8s.configmap.mount.path", "/etc/config")))
  
  // 验证Secret属性
  assert_true(config_attributes.contains(("k8s.secret.name", "payment-service-secrets")))
  assert_true(config_attributes.contains(("k8s.secret.namespace", "production")))
  assert_true(config_attributes.contains(("k8s.secret.uid", "c3d4e5f6-g7h8-9012-cdef-234567890123")))
  assert_true(config_attributes.contains(("k8s.secret.mount.path", "/etc/secrets")))
  
  // 验证配置数据键
  assert_true(config_attributes.contains(("k8s.configmap.key", "database.host")))
  assert_true(config_attributes.contains(("k8s.configmap.key", "database.port")))
  assert_true(config_attributes.contains(("k8s.configmap.key", "cache.ttl")))
  assert_true(config_attributes.contains(("k8s.configmap.key", "log.level")))
  
  // 验证密钥数据键
  assert_true(config_attributes.contains(("k8s.secret.key", "database.username")))
  assert_true(config_attributes.contains(("k8s.secret.key", "database.password")))
  assert_true(config_attributes.contains(("k8s.secret.key", "api.key")))
}