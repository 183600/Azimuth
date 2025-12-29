// 遥测跨平台兼容性测试用例
// 测试遥测系统在不同平台和操作系统上的兼容性

test "telemetry_cross_platform_operating_systems" {
  // 测试跨操作系统兼容性
  
  let operating_systems = [
    ("Linux", "Ubuntu 22.04", "x86_64", true),
    ("Linux", "CentOS 8", "x86_64", true),
    ("Linux", "Debian 11", "ARM64", true),
    ("Windows", "Windows Server 2019", "x86_64", true),
    ("Windows", "Windows 11", "x86_64", true),
    ("macOS", "Monterey", "x86_64", true),
    ("macOS", "Monterey", "ARM64", true),
    ("FreeBSD", "13.0", "x86_64", false)
  ]
  
  // 验证操作系统数量
  assert_eq(operating_systems.length(), 8)
  
  // 验证Linux Ubuntu支持
  assert_eq(operating_systems[0].0, "Linux")
  assert_eq(operating_systems[0].1, "Ubuntu 22.04")
  assert_eq(operating_systems[0].2, "x86_64")
  assert_eq(operating_systems[0].3, true)
  
  // 验证Linux CentOS支持
  assert_eq(operating_systems[1].0, "Linux")
  assert_eq(operating_systems[1].1, "CentOS 8")
  assert_eq(operating_systems[1].2, "x86_64")
  assert_eq(operating_systems[1].3, true)
  
  // 验证Linux Debian ARM64支持
  assert_eq(operating_systems[2].0, "Linux")
  assert_eq(operating_systems[2].1, "Debian 11")
  assert_eq(operating_systems[2].2, "ARM64")
  assert_eq(operating_systems[2].3, true)
  
  // 验证Windows Server支持
  assert_eq(operating_systems[3].0, "Windows")
  assert_eq(operating_systems[3].1, "Windows Server 2019")
  assert_eq(operating_systems[3].2, "x86_64")
  assert_eq(operating_systems[3].3, true)
  
  // 验证Windows 11支持
  assert_eq(operating_systems[4].0, "Windows")
  assert_eq(operating_systems[4].1, "Windows 11")
  assert_eq(operating_systems[4].2, "x86_64")
  assert_eq(operating_systems[4].3, true)
  
  // 验证macOS x86_64支持
  assert_eq(operating_systems[5].0, "macOS")
  assert_eq(operating_systems[5].1, "Monterey")
  assert_eq(operating_systems[5].2, "x86_64")
  assert_eq(operating_systems[5].3, true)
  
  // 验证macOS ARM64支持
  assert_eq(operating_systems[6].0, "macOS")
  assert_eq(operating_systems[6].1, "Monterey")
  assert_eq(operating_systems[6].2, "ARM64")
  assert_eq(operating_systems[6].3, true)
  
  // 验证FreeBSD不支持
  assert_eq(operating_systems[7].0, "FreeBSD")
  assert_eq(operating_systems[7].1, "13.0")
  assert_eq(operating_systems[7].2, "x86_64")
  assert_eq(operating_systems[7].3, false)
  
  // 计算支持的操作系统数量
  let mut supported_os_count = 0
  let mut i = 0
  while i < operating_systems.length() {
    if operating_systems[i].3 == true {
      supported_os_count = supported_os_count + 1
    }
    i = i + 1
  }
  
  assert_eq(supported_os_count, 7)
  
  // 计算不支持的操作系统数量
  let mut unsupported_os_count = 0
  i = 0
  while i < operating_systems.length() {
    if operating_systems[i].3 == false {
      unsupported_os_count = unsupported_os_count + 1
    }
    i = i + 1
  }
  
  assert_eq(unsupported_os_count, 1)
  
  // 计算Linux发行版数量
  let mut linux_distros_count = 0
  i = 0
  while i < operating_systems.length() {
    if operating_systems[i].0 == "Linux" {
      linux_distros_count = linux_distros_count + 1
    }
    i = i + 1
  }
  
  assert_eq(linux_distros_count, 3)
  
  // 计算x86_64架构数量
  let mut x86_64_count = 0
  i = 0
  while i < operating_systems.length() {
    if operating_systems[i].2 == "x86_64" {
      x86_64_count = x86_64_count + 1
    }
    i = i + 1
  }
  
  assert_eq(x86_64_count, 5)
  
  // 计算ARM64架构数量
  let mut arm64_count = 0
  i = 0
  while i < operating_systems.length() {
    if operating_systems[i].2 == "ARM64" {
      arm64_count = arm64_count + 1
    }
    i = i + 1
  }
  
  assert_eq(arm64_count, 2)
}

test "telemetry_cross_platform_runtime_environments" {
  // 测试跨运行时环境兼容性
  
  let runtime_environments = [
    ("Node.js", "18.x", "JavaScript", true),
    ("Node.js", "16.x", "JavaScript", true),
    ("Node.js", "14.x", "JavaScript", false),
    ("Python", "3.11", "Python", true),
    ("Python", "3.10", "Python", true),
    ("Python", "3.9", "Python", true),
    ("Python", "3.8", "Python", false),
    ("Java", "17", "Java", true),
    ("Java", "11", "Java", true),
    ("Java", "8", "Java", false),
    ("Go", "1.20", "Go", true),
    ("Go", "1.19", "Go", true),
    ("Go", "1.18", "Go", false),
    (.NET", "7.0", "C#", true),
    (.NET", "6.0", "C#", true),
    (.NET", "5.0", "C#", false)
  ]
  
  // 验证运行时环境数量
  assert_eq(runtime_environments.length(), 15)
  
  // 验证Node.js 18.x支持
  assert_eq(runtime_environments[0].0, "Node.js")
  assert_eq(runtime_environments[0].1, "18.x")
  assert_eq(runtime_environments[0].2, "JavaScript")
  assert_eq(runtime_environments[0].3, true)
  
  // 验证Node.js 14.x不支持
  assert_eq(runtime_environments[2].0, "Node.js")
  assert_eq(runtime_environments[2].1, "14.x")
  assert_eq(runtime_environments[2].2, "JavaScript")
  assert_eq(runtime_environments[2].3, false)
  
  // 验证Python 3.11支持
  assert_eq(runtime_environments[3].0, "Python")
  assert_eq(runtime_environments[3].1, "3.11")
  assert_eq(runtime_environments[3].2, "Python")
  assert_eq(runtime_environments[3].3, true)
  
  // 验证Python 3.8不支持
  assert_eq(runtime_environments[6].0, "Python")
  assert_eq(runtime_environments[6].1, "3.8")
  assert_eq(runtime_environments[6].2, "Python")
  assert_eq(runtime_environments[6].3, false)
  
  // 验证Java 17支持
  assert_eq(runtime_environments[7].0, "Java")
  assert_eq(runtime_environments[7].1, "17")
  assert_eq(runtime_environments[7].2, "Java")
  assert_eq(runtime_environments[7].3, true)
  
  // 验证Java 8不支持
  assert_eq(runtime_environments[9].0, "Java")
  assert_eq(runtime_environments[9].1, "8")
  assert_eq(runtime_environments[9].2, "Java")
  assert_eq(runtime_environments[9].3, false)
  
  // 验证Go 1.20支持
  assert_eq(runtime_environments[10].0, "Go")
  assert_eq(runtime_environments[10].1, "1.20")
  assert_eq(runtime_environments[10].2, "Go")
  assert_eq(runtime_environments[10].3, true)
  
  // 验证Go 1.18不支持
  assert_eq(runtime_environments[12].0, "Go")
  assert_eq(runtime_environments[12].1, "1.18")
  assert_eq(runtime_environments[12].2, "Go")
  assert_eq(runtime_environments[12].3, false)
  
  // 验证.NET 7.0支持
  assert_eq(runtime_environments[13].0, ".NET")
  assert_eq(runtime_environments[13].1, "7.0")
  assert_eq(runtime_environments[13].2, "C#")
  assert_eq(runtime_environments[13].3, true)
  
  // 验证.NET 5.0不支持
  assert_eq(runtime_environments[14].0, ".NET")
  assert_eq(runtime_environments[14].1, "5.0")
  assert_eq(runtime_environments[14].2, "C#")
  assert_eq(runtime_environments[14].3, false)
  
  // 计算支持的运行时环境数量
  let mut supported_runtimes_count = 0
  let mut i = 0
  while i < runtime_environments.length() {
    if runtime_environments[i].3 == true {
      supported_runtimes_count = supported_runtimes_count + 1
    }
    i = i + 1
  }
  
  assert_eq(supported_runtimes_count, 10)
  
  // 计算不支持的运行时环境数量
  let mut unsupported_runtimes_count = 0
  i = 0
  while i < runtime_environments.length() {
    if runtime_environments[i].3 == false {
      unsupported_runtimes_count = unsupported_runtimes_count + 1
    }
    i = i + 1
  }
  
  assert_eq(unsupported_runtimes_count, 5)
  
  // 按编程语言统计
  let language_counts = [
    ("JavaScript", 3),
    ("Python", 4),
    ("Java", 3),
    ("Go", 3),
    ("C#", 3)
  ]
  
  // 验证语言统计
  assert_eq(language_counts.length(), 5)
  assert_eq(language_counts[0].0, "JavaScript")
  assert_eq(language_counts[0].1, 3)
  assert_eq(language_counts[1].0, "Python")
  assert_eq(language_counts[1].1, 4)
}

test "telemetry_cross_platform_cloud_providers" {
  // 测试跨云提供商兼容性
  
  let cloud_providers = [
    ("AWS", "EC2", "us-east-1", true),
    ("AWS", "ECS", "us-west-2", true),
    ("AWS", "EKS", "eu-west-1", true),
    ("AWS", "Lambda", "ap-southeast-1", true),
    ("GCP", "Compute Engine", "us-central1", true),
    ("GCP", "GKE", "europe-west1", true),
    ("GCP", "Cloud Run", "asia-east1", true),
    ("GCP", "Cloud Functions", "us-west1", true),
    ("Azure", "Virtual Machines", "eastus", true),
    ("Azure", "Container Instances", "westeurope", true),
    ("Azure", "AKS", "southeastasia", true),
    ("Azure", "Functions", "eastus2", true),
    ("Alibaba Cloud", "ECS", "cn-hangzhou", false),
    ("Tencent Cloud", "CVM", "ap-beijing", false)
  ]
  
  // 验证云提供商数量
  assert_eq(cloud_providers.length(), 14)
  
  // 验证AWS EC2支持
  assert_eq(cloud_providers[0].0, "AWS")
  assert_eq(cloud_providers[0].1, "EC2")
  assert_eq(cloud_providers[0].2, "us-east-1")
  assert_eq(cloud_providers[0].3, true)
  
  // 验证AWS Lambda支持
  assert_eq(cloud_providers[3].0, "AWS")
  assert_eq(cloud_providers[3].1, "Lambda")
  assert_eq(cloud_providers[3].2, "ap-southeast-1")
  assert_eq(cloud_providers[3].3, true)
  
  // 验证GCP Compute Engine支持
  assert_eq(cloud_providers[4].0, "GCP")
  assert_eq(cloud_providers[4].1, "Compute Engine")
  assert_eq(cloud_providers[4].2, "us-central1")
  assert_eq(cloud_providers[4].3, true)
  
  // 验证GCP Cloud Functions支持
  assert_eq(cloud_providers[7].0, "GCP")
  assert_eq(cloud_providers[7].1, "Cloud Functions")
  assert_eq(cloud_providers[7].2, "us-west1")
  assert_eq(cloud_providers[7].3, true)
  
  // 验证Azure Virtual Machines支持
  assert_eq(cloud_providers[8].0, "Azure")
  assert_eq(cloud_providers[8].1, "Virtual Machines")
  assert_eq(cloud_providers[8].2, "eastus")
  assert_eq(cloud_providers[8].3, true)
  
  // 验证Azure Functions支持
  assert_eq(cloud_providers[11].0, "Azure")
  assert_eq(cloud_providers[11].1, "Functions")
  assert_eq(cloud_providers[11].2, "eastus2")
  assert_eq(cloud_providers[11].3, true)
  
  // 验证阿里云不支持
  assert_eq(cloud_providers[12].0, "Alibaba Cloud")
  assert_eq(cloud_providers[12].1, "ECS")
  assert_eq(cloud_providers[12].2, "cn-hangzhou")
  assert_eq(cloud_providers[12].3, false)
  
  // 验证腾讯云不支持
  assert_eq(cloud_providers[13].0, "Tencent Cloud")
  assert_eq(cloud_providers[13].1, "CVM")
  assert_eq(cloud_providers[13].2, "ap-beijing")
  assert_eq(cloud_providers[13].3, false)
  
  // 计算支持的云服务数量
  let mut supported_services_count = 0
  let mut i = 0
  while i < cloud_providers.length() {
    if cloud_providers[i].3 == true {
      supported_services_count = supported_services_count + 1
    }
    i = i + 1
  }
  
  assert_eq(supported_services_count, 12)
  
  // 计算不支持的云服务数量
  let mut unsupported_services_count = 0
  i = 0
  while i < cloud_providers.length() {
    if cloud_providers[i].3 == false {
      unsupported_services_count = unsupported_services_count + 1
    }
    i = i + 1
  }
  
  assert_eq(unsupported_services_count, 2)
  
  // 按云提供商统计
  let provider_counts = [
    ("AWS", 4),
    ("GCP", 4),
    ("Azure", 4),
    ("Alibaba Cloud", 1),
    ("Tencent Cloud", 1)
  ]
  
  // 验证提供商统计
  assert_eq(provider_counts.length(), 5)
  assert_eq(provider_counts[0].0, "AWS")
  assert_eq(provider_counts[0].1, 4)
  assert_eq(provider_counts[1].0, "GCP")
  assert_eq(provider_counts[1].1, 4)
  assert_eq(provider_counts[2].0, "Azure")
  assert_eq(provider_counts[2].1, 4)
}

test "telemetry_cross_platform_container_orchestrators" {
  // 测试跨容器编排器兼容性
  
  let container_orchestrators = [
    ("Kubernetes", "1.25", "Docker", true),
    ("Kubernetes", "1.24", "containerd", true),
    ("Kubernetes", "1.23", "CRI-O", true),
    ("Kubernetes", "1.22", "Docker", false),
    ("Docker Swarm", "latest", "Docker", true),
    ("OpenShift", "4.11", "CRI-O", true),
    ("OpenShift", "4.10", "CRI-O", true),
    ("OpenShift", "4.9", "CRI-O", false),
    ("Nomad", "1.4", "Docker", true),
    ("Nomad", "1.3", "containerd", true),
    ("Nomad", "1.2", "Docker", false),
    ("ECS", "latest", "Docker", true)
  ]
  
  // 验证容器编排器数量
  assert_eq(container_orchestrators.length(), 12)
  
  // 验证Kubernetes 1.25支持
  assert_eq(container_orchestrators[0].0, "Kubernetes")
  assert_eq(container_orchestrators[0].1, "1.25")
  assert_eq(container_orchestrators[0].2, "Docker")
  assert_eq(container_orchestrators[0].3, true)
  
  // 验证Kubernetes 1.22不支持
  assert_eq(container_orchestrators[3].0, "Kubernetes")
  assert_eq(container_orchestrators[3].1, "1.22")
  assert_eq(container_orchestrators[3].2, "Docker")
  assert_eq(container_orchestrators[3].3, false)
  
  // 验证Docker Swarm支持
  assert_eq(container_orchestrators[4].0, "Docker Swarm")
  assert_eq(container_orchestrators[4].1, "latest")
  assert_eq(container_orchestrators[4].2, "Docker")
  assert_eq(container_orchestrators[4].3, true)
  
  // 验证OpenShift 4.11支持
  assert_eq(container_orchestrators[5].0, "OpenShift")
  assert_eq(container_orchestrators[5].1, "4.11")
  assert_eq(container_orchestrators[5].2, "CRI-O")
  assert_eq(container_orchestrators[5].3, true)
  
  // 验证OpenShift 4.9不支持
  assert_eq(container_orchestrators[7].0, "OpenShift")
  assert_eq(container_orchestrators[7].1, "4.9")
  assert_eq(container_orchestrators[7].2, "CRI-O")
  assert_eq(container_orchestrators[7].3, false)
  
  // 验证Nomad 1.4支持
  assert_eq(container_orchestrators[8].0, "Nomad")
  assert_eq(container_orchestrators[8].1, "1.4")
  assert_eq(container_orchestrators[8].2, "Docker")
  assert_eq(container_orchestrators[8].3, true)
  
  // 验证Nomad 1.2不支持
  assert_eq(container_orchestrators[10].0, "Nomad")
  assert_eq(container_orchestrators[10].1, "1.2")
  assert_eq(container_orchestrators[10].2, "Docker")
  assert_eq(container_orchestrators[10].3, false)
  
  // 验证ECS支持
  assert_eq(container_orchestrators[11].0, "ECS")
  assert_eq(container_orchestrators[11].1, "latest")
  assert_eq(container_orchestrators[11].2, "Docker")
  assert_eq(container_orchestrators[11].3, true)
  
  // 计算支持的容器编排器数量
  let mut supported_orchestrators_count = 0
  let mut i = 0
  while i < container_orchestrators.length() {
    if container_orchestrators[i].3 == true {
      supported_orchestrators_count = supported_orchestrators_count + 1
    }
    i = i + 1
  }
  
  assert_eq(supported_orchestrators_count, 8)
  
  // 计算不支持的容器编排器数量
  let mut unsupported_orchestrators_count = 0
  i = 0
  while i < container_orchestrators.length() {
    if container_orchestrators[i].3 == false {
      unsupported_orchestrators_count = unsupported_orchestrators_count + 1
    }
    i = i + 1
  }
  
  assert_eq(unsupported_orchestrators_count, 4)
  
  // 按编排器类型统计
  let orchestrator_counts = [
    ("Kubernetes", 4),
    ("Docker Swarm", 1),
    ("OpenShift", 3),
    ("Nomad", 3),
    ("ECS", 1)
  ]
  
  // 验证编排器统计
  assert_eq(orchestrator_counts.length(), 5)
  assert_eq(orchestrator_counts[0].0, "Kubernetes")
  assert_eq(orchestrator_counts[0].1, 4)
  assert_eq(orchestrator_counts[1].0, "Docker Swarm")
  assert_eq(orchestrator_counts[1].1, 1)
  
  // 按容器运行时统计
  let runtime_counts = [
    ("Docker", 5),
    ("containerd", 2),
    ("CRI-O", 5)
  ]
  
  // 验证容器运行时统计
  assert_eq(runtime_counts.length(), 3)
  assert_eq(runtime_counts[0].0, "Docker")
  assert_eq(runtime_counts[0].1, 5)
  assert_eq(runtime_counts[1].0, "containerd")
  assert_eq(runtime_counts[1].1, 2)
  assert_eq(runtime_counts[2].0, "CRI-O")
  assert_eq(runtime_counts[2].1, 5)
}

test "telemetry_cross_platform_databases" {
  // 测试跨数据库兼容性
  
  let databases = [
    ("PostgreSQL", "14.x", "SQL", true),
    ("PostgreSQL", "13.x", "SQL", true),
    ("PostgreSQL", "12.x", "SQL", false),
    ("MySQL", "8.0", "SQL", true),
    ("MySQL", "5.7", "SQL", false),
    ("MongoDB", "6.0", "NoSQL", true),
    ("MongoDB", "5.0", "NoSQL", true),
    ("MongoDB", "4.4", "NoSQL", false),
    ("Redis", "7.0", "Key-Value", true),
    ("Redis", "6.2", "Key-Value", true),
    ("Redis", "6.0", "Key-Value", false),
    ("Elasticsearch", "8.5", "Search", true),
    ("Elasticsearch", "8.0", "Search", true),
    ("Elasticsearch", "7.17", "Search", false),
    ("InfluxDB", "2.5", "Time Series", true),
    ("InfluxDB", "2.4", "Time Series", true),
    ("InfluxDB", "1.8", "Time Series", false)
  ]
  
  // 验证数据库数量
  assert_eq(databases.length(), 17)
  
  // 验证PostgreSQL 14.x支持
  assert_eq(databases[0].0, "PostgreSQL")
  assert_eq(databases[0].1, "14.x")
  assert_eq(databases[0].2, "SQL")
  assert_eq(databases[0].3, true)
  
  // 验证PostgreSQL 12.x不支持
  assert_eq(databases[2].0, "PostgreSQL")
  assert_eq(databases[2].1, "12.x")
  assert_eq(databases[2].2, "SQL")
  assert_eq(databases[2].3, false)
  
  // 验证MySQL 8.0支持
  assert_eq(databases[3].0, "MySQL")
  assert_eq(databases[3].1, "8.0")
  assert_eq(databases[3].2, "SQL")
  assert_eq(databases[3].3, true)
  
  // 验证MySQL 5.7不支持
  assert_eq(databases[4].0, "MySQL")
  assert_eq(databases[4].1, "5.7")
  assert_eq(databases[4].2, "SQL")
  assert_eq(databases[4].3, false)
  
  // 验证MongoDB 6.0支持
  assert_eq(databases[5].0, "MongoDB")
  assert_eq(databases[5].1, "6.0")
  assert_eq(databases[5].2, "NoSQL")
  assert_eq(databases[5].3, true)
  
  // 验证MongoDB 4.4不支持
  assert_eq(databases[7].0, "MongoDB")
  assert_eq(databases[7].1, "4.4")
  assert_eq(databases[7].2, "NoSQL")
  assert_eq(databases[7].3, false)
  
  // 验证Redis 7.0支持
  assert_eq(databases[8].0, "Redis")
  assert_eq(databases[8].1, "7.0")
  assert_eq(databases[8].2, "Key-Value")
  assert_eq(databases[8].3, true)
  
  // 验证Redis 6.0不支持
  assert_eq(databases[10].0, "Redis")
  assert_eq(databases[10].1, "6.0")
  assert_eq(databases[10].2, "Key-Value")
  assert_eq(databases[10].3, false)
  
  // 验证Elasticsearch 8.5支持
  assert_eq(databases[11].0, "Elasticsearch")
  assert_eq(databases[11].1, "8.5")
  assert_eq(databases[11].2, "Search")
  assert_eq(databases[11].3, true)
  
  // 验证Elasticsearch 7.17不支持
  assert_eq(databases[13].0, "Elasticsearch")
  assert_eq(databases[13].1, "7.17")
  assert_eq(databases[13].2, "Search")
  assert_eq(databases[13].3, false)
  
  // 验证InfluxDB 2.5支持
  assert_eq(databases[14].0, "InfluxDB")
  assert_eq(databases[14].1, "2.5")
  assert_eq(databases[14].2, "Time Series")
  assert_eq(databases[14].3, true)
  
  // 验证InfluxDB 1.8不支持
  assert_eq(databases[16].0, "InfluxDB")
  assert_eq(databases[16].1, "1.8")
  assert_eq(databases[16].2, "Time Series")
  assert_eq(databases[16].3, false)
  
  // 计算支持的数据库数量
  let mut supported_databases_count = 0
  let mut i = 0
  while i < databases.length() {
    if databases[i].3 == true {
      supported_databases_count = supported_databases_count + 1
    }
    i = i + 1
  }
  
  assert_eq(supported_databases_count, 11)
  
  // 计算不支持的数据库数量
  let mut unsupported_databases_count = 0
  i = 0
  while i < databases.length() {
    if databases[i].3 == false {
      unsupported_databases_count = unsupported_databases_count + 1
    }
    i = i + 1
  }
  
  assert_eq(unsupported_databases_count, 6)
  
  // 按数据库类型统计
  let database_type_counts = [
    ("SQL", 5),
    ("NoSQL", 3),
    ("Key-Value", 3),
    ("Search", 3),
    ("Time Series", 3)
  ]
  
  // 验证数据库类型统计
  assert_eq(database_type_counts.length(), 5)
  assert_eq(database_type_counts[0].0, "SQL")
  assert_eq(database_type_counts[0].1, 5)
  assert_eq(database_type_counts[1].0, "NoSQL")
  assert_eq(database_type_counts[1].1, 3)
}

test "telemetry_cross_platform_message_queues" {
  // 测试跨消息队列兼容性
  
  let message_queues = [
    ("Apache Kafka", "3.4", true),
    ("Apache Kafka", "3.3", true),
    ("Apache Kafka", "3.2", false),
    ("RabbitMQ", "3.11", true),
    ("RabbitMQ", "3.10", true),
    ("RabbitMQ", "3.9", false),
    ("Apache ActiveMQ", "5.17", true),
    ("Apache ActiveMQ", "5.16", false),
    ("Amazon SQS", "latest", true),
    ("Google Pub/Sub", "latest", true),
    ("Azure Service Bus", "latest", true),
    ("NATS", "2.9", true),
    ("NATS", "2.8", true),
    ("NATS", "2.7", false),
    ("Redis Streams", "7.0", true),
    ("Redis Streams", "6.2", true),
    ("Redis Streams", "6.0", false)
  ]
  
  // 验证消息队列数量
  assert_eq(message_queues.length(), 17)
  
  // 验证Apache Kafka 3.4支持
  assert_eq(message_queues[0].0, "Apache Kafka")
  assert_eq(message_queues[0].1, "3.4")
  assert_eq(message_queues[0].2, true)
  
  // 验证Apache Kafka 3.2不支持
  assert_eq(message_queues[2].0, "Apache Kafka")
  assert_eq(message_queues[2].1, "3.2")
  assert_eq(message_queues[2].2, false)
  
  // 验证RabbitMQ 3.11支持
  assert_eq(message_queues[3].0, "RabbitMQ")
  assert_eq(message_queues[3].1, "3.11")
  assert_eq(message_queues[3].2, true)
  
  // 验证RabbitMQ 3.9不支持
  assert_eq(message_queues[5].0, "RabbitMQ")
  assert_eq(message_queues[5].1, "3.9")
  assert_eq(message_queues[5].2, false)
  
  // 验证Apache ActiveMQ 5.17支持
  assert_eq(message_queues[6].0, "Apache ActiveMQ")
  assert_eq(message_queues[6].1, "5.17")
  assert_eq(message_queues[6].2, true)
  
  // 验证Apache ActiveMQ 5.16不支持
  assert_eq(message_queues[7].0, "Apache ActiveMQ")
  assert_eq(message_queues[7].1, "5.16")
  assert_eq(message_queues[7].2, false)
  
  // 验证云消息队列支持
  assert_eq(message_queues[8].0, "Amazon SQS")
  assert_eq(message_queues[8].2, true)
  
  assert_eq(message_queues[9].0, "Google Pub/Sub")
  assert_eq(message_queues[9].2, true)
  
  assert_eq(message_queues[10].0, "Azure Service Bus")
  assert_eq(message_queues[10].2, true)
  
  // 验证NATS 2.9支持
  assert_eq(message_queues[11].0, "NATS")
  assert_eq(message_queues[11].1, "2.9")
  assert_eq(message_queues[11].2, true)
  
  // 验证NATS 2.7不支持
  assert_eq(message_queues[13].0, "NATS")
  assert_eq(message_queues[13].1, "2.7")
  assert_eq(message_queues[13].2, false)
  
  // 验证Redis Streams 7.0支持
  assert_eq(message_queues[14].0, "Redis Streams")
  assert_eq(message_queues[14].1, "7.0")
  assert_eq(message_queues[14].2, true)
  
  // 验证Redis Streams 6.0不支持
  assert_eq(message_queues[16].0, "Redis Streams")
  assert_eq(message_queues[16].1, "6.0")
  assert_eq(message_queues[16].2, false)
  
  // 计算支持的消息队列数量
  let mut supported_queues_count = 0
  let mut i = 0
  while i < message_queues.length() {
    if message_queues[i].2 == true {
      supported_queues_count = supported_queues_count + 1
    }
    i = i + 1
  }
  
  assert_eq(supported_queues_count, 11)
  
  // 计算不支持的消息队列数量
  let mut unsupported_queues_count = 0
  i = 0
  while i < message_queues.length() {
    if message_queues[i].2 == false {
      unsupported_queues_count = unsupported_queues_count + 1
    }
    i = i + 1
  }
  
  assert_eq(unsupported_queues_count, 6)
  
  // 按消息队列类型统计
  let queue_type_counts = [
    ("Traditional", 8),    // Kafka, RabbitMQ, ActiveMQ
    ("Cloud", 3),          // SQS, Pub/Sub, Service Bus
    ("Lightweight", 3),    // NATS
    ("Redis-based", 3)     // Redis Streams
  ]
  
  // 验证消息队列类型统计
  assert_eq(queue_type_counts.length(), 4)
  assert_eq(queue_type_counts[0].0, "Traditional")
  assert_eq(queue_type_counts[0].1, 8)
  assert_eq(queue_type_counts[1].0, "Cloud")
  assert_eq(queue_type_counts[1].1, 3)
}