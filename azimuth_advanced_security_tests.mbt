// Azimuth 高级安全测试用例
// 专注于遥测系统中的安全威胁检测和防护功能

// 测试1: 异常访问模式检测
test "异常访问模式检测" {
  // 模拟用户访问行为数据
  let access_patterns = [
    { 
      timestamp: 1640995200, 
      user_id: "user001", 
      ip_address: "192.168.1.100", 
      user_agent: "Mozilla/5.0", 
      endpoint: "/api/orders", 
      method: "GET", 
      status_code: 200,
      response_time_ms: 120
    },
    { 
      timestamp: 1640995205, 
      user_id: "user001", 
      ip_address: "192.168.1.100", 
      user_agent: "Mozilla/5.0", 
      endpoint: "/api/orders/123", 
      method: "GET", 
      status_code: 200,
      response_time_ms: 95
    },
    { 
      timestamp: 1640995210, 
      user_id: "user002", 
      ip_address: "10.0.0.50", 
      user_agent: "Mozilla/5.0", 
      endpoint: "/api/products", 
      method: "GET", 
      status_code: 200,
      response_time_ms: 80
    },
    { 
      timestamp: 1640995215, 
      user_id: "user003", 
      ip_address: "203.0.113.45", 
      user_agent: "curl/7.68.0", 
      endpoint: "/api/admin/users", 
      method: "GET", 
      status_code: 403,
      response_time_ms: 25
    },
    { 
      timestamp: 1640995220, 
      user_id: "user003", 
      ip_address: "203.0.113.45", 
      user_agent: "curl/7.68.0", 
      endpoint: "/api/admin/users", 
      method: "GET", 
      status_code: 403,
      response_time_ms: 22
    },
    { 
      timestamp: 1640995225, 
      user_id: "user003", 
      ip_address: "203.0.113.45", 
      user_agent: "curl/7.68.0", 
      endpoint: "/api/admin/config", 
      method: "GET", 
      status_code: 403,
      response_time_ms: 20
    },
    { 
      timestamp: 1640995230, 
      user_id: "user004", 
      ip_address: "198.51.100.22", 
      user_agent: "Python-urllib/3.8", 
      endpoint: "/api/login", 
      method: "POST", 
      status_code: 401,
      response_time_ms: 150
    },
    { 
      timestamp: 1640995235, 
      user_id: "user004", 
      ip_address: "198.51.100.22", 
      user_agent: "Python-urllib/3.8", 
      endpoint: "/api/login", 
      method: "POST", 
      status_code: 401,
      response_time_ms: 145
    },
    { 
      timestamp: 1640995240, 
      user_id: "user004", 
      ip_address: "198.51.100.22", 
      user_agent: "Python-urllib/3.8", 
      endpoint: "/api/login", 
      method: "POST", 
      status_code: 401,
      response_time_ms: 148
    },
    { 
      timestamp: 1640995245, 
      user_id: "user004", 
      ip_address: "198.51.100.22", 
      user_agent: "Python-urllib/3.8", 
      endpoint: "/api/login", 
      method: "POST", 
      status_code: 401,
      response_time_ms: 152
    },
    { 
      timestamp: 1640995250, 
      user_id: "user004", 
      ip_address: "198.51.100.22", 
      user_agent: "Python-urllib/3.8", 
      endpoint: "/api/login", 
      method: "POST", 
      status_code: 401,
      response_time_ms: 149
    }
  ]
  
  // 检测暴力破解攻击
  let detect_brute_force_attack = fn(patterns: Array[AccessPattern>, threshold: Int, time_window_seconds: Int) {
    let mut brute_force_attempts = []
    
    // 按IP地址分组
    let ip_groups = patterns.group_by(fn(p) { p.ip_address })
    
    for (ip_address, ip_patterns) in ip_groups {
      // 按端点分组
      let endpoint_groups = ip_patterns.group_by(fn(p) { p.endpoint })
      
      for (endpoint, endpoint_patterns) in endpoint_groups {
        // 筛选失败请求
        let failed_attempts = endpoint_patterns.filter_fn(p) { p.status_code >= 400 }
        
        if failed_attempts.length() >= threshold {
          let time_span = failed_attempts[failed_attempts.length() - 1].timestamp - failed_attempts[0].timestamp
          
          if time_span <= time_window_seconds {
            brute_force_attempts = brute_force_attempts.push({
              ip_address: ip_address,
              endpoint: endpoint,
              attempt_count: failed_attempts.length(),
              time_span_seconds: time_span,
              first_attempt: failed_attempts[0].timestamp,
              last_attempt: failed_attempts[failed_attempts.length() - 1].timestamp,
              user_agents: failed_attempts.map_fn(p) { p.user_agent }.unique(),
              severity: if failed_attempts.length() >= threshold * 2 { "critical" } else { "warning" }
            })
          }
        }
      }
    }
    
    brute_force_attempts
  }
  
  // 检测异常访问模式
  let detect_anomalous_access = fn(patterns: Array[AccessPattern>) {
    let mut anomalies = []
    
    // 按用户分组
    let user_groups = patterns.group_by(fn(p) { p.user_id })
    
    for (user_id, user_patterns) in user_groups {
      // 检测短时间内大量请求
      if user_patterns.length() > 10 {
        let time_span = user_patterns[user_patterns.length() - 1].timestamp - user_patterns[0].timestamp
        
        if time_span < 60 {  // 1分钟内超过10个请求
          anomalies = anomalies.push({
            user_id: user_id,
            anomaly_type: "high_frequency_requests",
            request_count: user_patterns.length(),
            time_span_seconds: time_span,
            endpoints: user_patterns.map_fn(p) { p.endpoint }.unique(),
            severity: "warning"
          })
        }
      }
      
      // 检测异常端点访问
      let admin_endpoints = user_patterns.filter_fn(p) { p.endpoint.contains("/admin") }
      
      if admin_endpoints.length() > 0 {
        let failed_admin_attempts = admin_endpoints.filter_fn(p) { p.status_code >= 400 }
        
        if failed_admin_attempts.length() > 0 {
          anomalies = anomalies.push({
            user_id: user_id,
            anomaly_type: "unauthorized_admin_access",
            attempt_count: failed_admin_attempts.length(),
            endpoints: admin_endpoints.map_fn(p) { p.endpoint },
            ip_addresses: admin_endpoints.map_fn(p) { p.ip_address }.unique(),
            severity: "critical"
          })
        }
      }
      
      // 检测异常User-Agent
      let user_agents = user_patterns.map_fn(p) { p.user_agent }.unique()
      let suspicious_agents = user_agents.filter_fn(ua) { 
        ua.contains("curl") or ua.contains("python") or ua.contains("wget")
      }
      
      if suspicious_agents.length() > 0 {
        let suspicious_requests = user_patterns.filter_fn(p) { 
          suspicious_agents.contains(p.user_agent)
        }
        
        if suspicious_requests.length() > 3 {  // 超过3个可疑请求
          anomalies = anomalies.push({
            user_id: user_id,
            anomaly_type: "suspicious_user_agent",
            request_count: suspicious_requests.length(),
            user_agents: suspicious_agents,
            endpoints: suspicious_requests.map_fn(p) { p.endpoint }.unique(),
            severity: "warning"
          })
        }
      }
    }
    
    anomalies
  }
  
  // 检测地理位置异常
  let detect_geographic_anomalies = fn(patterns: Array[AccessPattern>) {
    let mut geo_anomalies = []
    
    // 模拟IP地理位置映射
    let ip_to_geo = [
      { ip: "192.168.1.100", country: "US", city: "New York" },
      { ip: "10.0.0.50", country: "US", city: "San Francisco" },
      { ip: "203.0.113.45", country: "CN", city: "Beijing" },
      { ip: "198.51.100.22", country: "RU", city: "Moscow" }
    ]
    
    // 按用户分组
    let user_groups = patterns.group_by(fn(p) { p.user_id })
    
    for (user_id, user_patterns) in user_groups {
      // 获取用户的所有IP地址
      let user_ips = user_patterns.map_fn(p) { p.ip_address }.unique()
      
      if user_ips.length() > 1 {
        let mut user_countries = []
        
        for ip in user_ips {
          let geo_info = ip_to_geo.find_fn(g) { g.ip == ip }
          if geo_info.is_some() {
            user_countries = user_countries.push(geo_info.unwrap().country)
          }
        }
        
        let unique_countries = user_countries.unique()
        
        if unique_countries.length() > 1 {
          geo_anomalies = geo_anomalies.push({
            user_id: user_id,
            anomaly_type: "impossible_travel",
            ip_addresses: user_ips,
            countries: unique_countries,
            request_count: user_patterns.length(),
            severity: "critical"
          })
        }
      }
    }
    
    geo_anomalies
  }
  
  // 执行安全检测
  let brute_force_attempts = detect_brute_force_attack(access_patterns, 3, 60)
  let anomalous_access = detect_anomalous_access(access_patterns)
  let geographic_anomalies = detect_geographic_anomalies(access_patterns)
  
  // 验证暴力破解攻击检测
  assert_eq(brute_force_attempts.length(), 2)
  
  // 验证针对/api/login的暴力破解攻击
  let login_brute_force = brute_force_attempts.find_fn(bf) { bf.endpoint == "/api/login" }
  assert_true(login_brute_force.is_some())
  assert_eq(login_brute_force.unwrap().ip_address, "198.51.100.22")
  assert_eq(login_brute_force.unwrap().attempt_count, 5)
  assert_eq(login_brute_force.unwrap().time_span_seconds, 20)
  assert_eq(login_brute_force.unwrap().severity, "critical")
  assert_eq(login_brute_force.unwrap().user_agents.length(), 1)
  assert_true(login_brute_force.unwrap().user_agents.contains("Python-urllib/3.8"))
  
  // 验证针对管理端口的未授权访问
  let admin_brute_force = brute_force_attempts.find_fn(bf) { bf.endpoint.contains("/admin") }
  assert_true(admin_brute_force.is_some())
  assert_eq(admin_brute_force.unwrap().ip_address, "203.0.113.45")
  assert_eq(admin_brute_force.unwrap().attempt_count, 3)
  assert_eq(admin_brute_force.unwrap().severity, "warning")
  
  // 验证异常访问模式检测
  assert_eq(anomalous_access.length(), 3)
  
  // 验证高频请求检测
  let high_freq_anomaly = anomalous_access.find_fn(a) { a.anomaly_type == "high_frequency_requests" }
  assert_true(high_freq_anomaly.is_some())
  assert_eq(high_freq_anomaly.unwrap().user_id, "user004")
  assert_eq(high_freq_anomaly.unwrap().request_count, 5)
  assert_eq(high_freq_anomaly.unwrap().severity, "warning")
  
  // 验证未授权管理员访问检测
  let admin_access_anomaly = anomalous_access.find_fn(a) { a.anomaly_type == "unauthorized_admin_access" }
  assert_true(admin_access_anomaly.is_some())
  assert_eq(admin_access_anomaly.unwrap().user_id, "user003")
  assert_eq(admin_access_anomaly.unwrap().attempt_count, 3)
  assert_eq(admin_access_anomaly.unwrap().severity, "critical")
  
  // 验证可疑User-Agent检测
  let suspicious_ua_anomaly = anomalous_access.find_fn(a) { a.anomaly_type == "suspicious_user_agent" }
  assert_true(suspicious_ua_anomaly.is_some())
  assert_eq(suspicious_ua_anomaly.unwrap().user_id, "user003")
  assert_eq(suspicious_ua_anomaly.unwrap().request_count, 3)
  assert_eq(suspicious_ua_anomaly.unwrap().severity, "warning")
  
  // 验证地理位置异常检测
  assert_eq(geographic_anomalies.length(), 1)
  
  let geo_anomaly = geographic_anomalies[0]
  assert_eq(geo_anomaly.user_id, "user003")
  assert_eq(geo_anomaly.anomaly_type, "impossible_travel")
  assert_eq(geo_anomaly.ip_addresses.length(), 1)
  assert_eq(geo_anomaly.countries.length(), 1)
  assert_true(geo_anomaly.countries.contains("CN"))
  assert_eq(geo_anomaly.severity, "critical")
}

// 测试2: 数据泄露检测
test "数据泄露检测" {
  // 模拟数据访问和传输记录
  let data_access_records = [
    { 
      timestamp: 1640995200, 
      user_id: "user001", 
      action: "access", 
      resource_type: "customer_data", 
      resource_id: "cust_001", 
      data_size_bytes: 1024,
      ip_address: "192.168.1.100",
      location: "office"
    },
    { 
      timestamp: 1640995210, 
      user_id: "user001", 
      action: "access", 
      resource_type: "customer_data", 
      resource_id: "cust_002", 
      data_size_bytes: 1024,
      ip_address: "192.168.1.100",
      location: "office"
    },
    { 
      timestamp: 1640995220, 
      user_id: "user002", 
      action: "download", 
      resource_type: "financial_report", 
      resource_id: "report_q1", 
      data_size_bytes: 5242880,  // 5MB
      ip_address: "192.168.1.101",
      location: "office"
    },
    { 
      timestamp: 1640995230, 
      user_id: "user003", 
      action: "access", 
      resource_type: "customer_data", 
      resource_id: "cust_003", 
      data_size_bytes: 1024,
      ip_address: "203.0.113.45",
      location: "remote"
    },
    { 
      timestamp: 1640995240, 
      user_id: "user003", 
      action: "download", 
      resource_type: "customer_data", 
      resource_id: "cust_003", 
      data_size_bytes: 2048,
      ip_address: "203.0.113.45",
      location: "remote"
    },
    { 
      timestamp: 1640995250, 
      user_id: "user003", 
      action: "download", 
      resource_type: "customer_data", 
      resource_id: "cust_004", 
      data_size_bytes: 2048,
      ip_address: "203.0.113.45",
      location: "remote"
    },
    { 
      timestamp: 1640995260, 
      user_id: "user003", 
      action: "download", 
      resource_type: "customer_data", 
      resource_id: "cust_005", 
      data_size_bytes: 2048,
      ip_address: "203.0.113.45",
      location: "remote"
    },
    { 
      timestamp: 1640995270, 
      user_id: "user004", 
      action: "access", 
      resource_type: "source_code", 
      resource_id: "auth_service", 
      data_size_bytes: 1024,
      ip_address: "198.51.100.22",
      location: "vpn"
    },
    { 
      timestamp: 1640995280, 
      user_id: "user004", 
      action: "download", 
      resource_type: "source_code", 
      resource_id: "auth_service", 
      data_size_bytes: 10485760,  // 10MB
      ip_address: "198.51.100.22",
      location: "vpn"
    },
    { 
      timestamp: 1640995290, 
      user_id: "user004", 
      action: "download", 
      resource_type: "source_code", 
      resource_id: "payment_service", 
      data_size_bytes: 20971520,  // 20MB
      ip_address: "198.51.100.22",
      location: "vpn"
    }
  ]
  
  // 检测异常数据访问模式
  let detect_abnormal_data_access = fn(records: Array[DataAccessRecord>) {
    let mut anomalies = []
    
    // 按用户分组
    let user_groups = records.group_by(fn(r) { r.user_id })
    
    for (user_id, user_records) in user_groups {
      // 计算用户访问的数据总量
      let total_data_accessed = user_records.map_fn(r) { r.data_size_bytes }.sum()
      
      // 计算访问的不同资源类型数量
      let resource_types = user_records.map_fn(r) { r.resource_type }.unique()
      
      // 计算下载操作数量
      let downloads = user_records.filter_fn(r) { r.action == "download" }
      
      // 计算从远程位置访问的记录
      let remote_access = user_records.filter_fn(r) { r.location != "office" }
      
      // 检测大量数据下载
      if total_data_accessed > 10485760 {  // 超过10MB
        anomalies = anomalies.push({
          user_id: user_id,
          anomaly_type: "large_data_download",
          total_bytes: total_data_accessed,
          download_count: downloads.length(),
          resource_types: resource_types,
          severity: if total_data_accessed > 52428800 { "critical" } else { "warning" }
        })
      }
      
      // 检测从多个位置访问敏感数据
      let locations = user_records.map_fn(r) { r.location }.unique()
      let sensitive_resources = user_records.filter_fn(r) { 
        r.resource_type == "customer_data" or r.resource_type == "source_code"
      }
      
      if sensitive_resources.length() > 0 and locations.length() > 1 {
        anomalies = anomalies.push({
          user_id: user_id,
          anomaly_type: "multi_location_sensitive_access",
          locations: locations,
          sensitive_access_count: sensitive_resources.length(),
          resource_types: sensitive_resources.map_fn(r) { r.resource_type }.unique(),
          severity: "warning"
        })
      }
      
      // 检测短时间内大量下载
      if downloads.length() > 2 {
        let download_timespan = downloads[downloads.length() - 1].timestamp - downloads[0].timestamp
        
        if download_timespan < 300 {  // 5分钟内
          anomalies = anomalies.push({
            user_id: user_id,
            anomaly_type: "burst_downloads",
            download_count: downloads.length(),
            timespan_seconds: download_timespan,
            total_bytes: downloads.map_fn(d) { d.data_size_bytes }.sum(),
            severity: "critical"
          })
        }
      }
      
      // 检测访问异常资源类型
      let unusual_resources = user_records.filter_fn(r) { 
        r.resource_type == "source_code" or r.resource_type == "encryption_keys"
      }
      
      if unusual_resources.length() > 0 {
        anomalies = anomalies.push({
          user_id: user_id,
          anomaly_type: "unusual_resource_access",
          resource_types: unusual_resources.map_fn(r) { r.resource_type }.unique(),
          access_count: unusual_resources.length(),
          total_bytes: unusual_resources.map_fn(r) { r.data_size_bytes }.sum(),
          severity: "critical"
        })
      }
    }
    
    anomalies
  }
  
  // 检测数据外泄模式
  let detect_data_exfiltration = fn(records: Array[DataAccessRecord>) {
    let mut exfiltration_indicators = []
    
    // 按IP地址分组
    let ip_groups = records.group_by(fn(r) { r.ip_address })
    
    for (ip_address, ip_records) in ip_groups {
      // 计算从该IP下载的数据总量
      let downloads = ip_records.filter_fn(r) { r.action == "download" }
      let total_downloaded = downloads.map_fn(d) { d.data_size_bytes }.sum()
      
      // 计算访问的不同用户数量
      let users = ip_records.map_fn(r) { r.user_id }.unique()
      
      // 计算访问的不同资源类型数量
      let resource_types = ip_records.map_fn(r) { r.resource_type }.unique()
      
      // 检测从单个IP大量下载
      if total_downloaded > 20971520 {  // 超过20MB
        exfiltration_indicators = exfiltration_indicators.push({
          indicator_type: "large_volume_from_single_ip",
          ip_address: ip_address,
          total_bytes: total_downloaded,
          user_count: users.length(),
          resource_types: resource_types,
          severity: "critical"
        })
      }
      
      // 检测从单个IP访问多个用户账户
      if users.length() > 1 and downloads.length() > 0 {
        exfiltration_indicators = exfiltration_indicators.push({
          indicator_type: "multiple_accounts_from_single_ip",
          ip_address: ip_address,
          user_count: users.length(),
          download_count: downloads.length(),
          total_bytes: total_downloaded,
          severity: "warning"
        })
      }
      
      // 检测从外部IP访问敏感资源
      let external_ip = not (ip_address.starts_with("192.168.") or ip_address.starts_with("10."))
      let sensitive_access = ip_records.filter_fn(r) { 
        r.resource_type == "customer_data" or r.resource_type == "source_code"
      }
      
      if external_ip and sensitive_access.length() > 0 {
        exfiltration_indicators = exfiltration_indicators.push({
          indicator_type: "external_access_to_sensitive_data",
          ip_address: ip_address,
          sensitive_access_count: sensitive_access.length(),
          total_bytes: sensitive_access.map_fn(r) { r.data_size_bytes }.sum(),
          severity: "critical"
        })
      }
    }
    
    exfiltration_indicators
  }
  
  // 计算数据泄露风险评分
  let calculate_data_leakage_risk = fn(access_anomalies: Array[DataAccessAnomaly>, exfiltration_indicators: Array[ExfiltrationIndicator>) {
    let mut user_risk_scores = []
    
    // 获取所有涉及的用户
    let all_users = []
    for anomaly in access_anomalies {
      if not all_users.contains(anomaly.user_id) {
        all_users = all_users.push(anomaly.user_id)
      }
    }
    
    for indicator in exfiltration_indicators {
      // 某些指标类型没有直接关联用户，跳过
      if indicator.has_field("user_id") and not all_users.contains(indicator.user_id) {
        all_users = all_users.push(indicator.user_id)
      }
    }
    
    for user_id in all_users {
      let mut risk_score = 0
      let mut risk_factors = []
      
      // 基于访问异常计算风险
      let user_anomalies = access_anomalies.filter_fn(a) { a.user_id == user_id }
      
      for anomaly in user_anomalies {
        match anomaly.anomaly_type {
          "large_data_download" => {
            risk_score = risk_score + 30
            risk_factors = risk_factors.push("large_data_download")
          }
          "multi_location_sensitive_access" => {
            risk_score = risk_score + 20
            risk_factors = risk_factors.push("multi_location_access")
          }
          "burst_downloads" => {
            risk_score = risk_score + 40
            risk_factors = risk_factors.push("burst_downloads")
          }
          "unusual_resource_access" => {
            risk_score = risk_score + 50
            risk_factors = risk_factors.push("unusual_resource_access")
          }
          _ => {}
        }
      }
      
      // 基于外泄指标计算风险
      // 注意：这里简化处理，实际中需要更复杂的关联逻辑
      
      let risk_level = 
        if risk_score >= 80 { "critical" }
        else if risk_score >= 50 { "high" }
        else if risk_score >= 20 { "medium" }
        else if risk_score > 0 { "low" }
        else { "minimal" }
      
      user_risk_scores = user_risk_scores.push({
        user_id: user_id,
        risk_score: risk_score,
        risk_level: risk_level,
        risk_factors: risk_factors
      })
    }
    
    // 按风险评分排序
    user_risk_scores.sort_by(fn(a, b) { b.risk_score - a.risk_score })
  }
  
  // 执行数据泄露检测
  let access_anomalies = detect_abnormal_data_access(data_access_records)
  let exfiltration_indicators = detect_data_exfiltration(data_access_records)
  let risk_scores = calculate_data_leakage_risk(access_anomalies, exfiltration_indicators)
  
  // 验证异常数据访问模式检测
  assert_eq(access_anomalies.length(), 5)
  
  // 验证大量数据下载检测
  let large_download_anomaly = access_anomalies.find_fn(a) { a.anomaly_type == "large_data_download" }
  assert_true(large_download_anomaly.is_some())
  assert_eq(large_download_anomaly.unwrap().user_id, "user004")
  assert_eq(large_download_anomaly.unwrap().total_bytes, 31457280)  // 10MB + 20MB
  assert_eq(large_download_anomaly.unwrap().severity, "critical")
  
  // 验证多位置敏感访问检测
  let multi_location_anomaly = access_anomalies.find_fn(a) { a.anomaly_type == "multi_location_sensitive_access" }
  assert_true(multi_location_anomaly.is_some())
  assert_eq(multi_location_anomaly.unwrap().user_id, "user003")
  assert_eq(multi_location_anomaly.unwrap().locations.length(), 1)
  assert_eq(multi_location_anomaly.unwrap().severity, "warning")
  
  // 验证突发下载检测
  let burst_download_anomaly = access_anomalies.find_fn(a) { a.anomaly_type == "burst_downloads" }
  assert_true(burst_download_anomaly.is_some())
  assert_eq(burst_download_anomaly.unwrap().user_id, "user003")
  assert_eq(burst_download_anomaly.unwrap().download_count, 3)
  assert_eq(burst_download_anomaly.unwrap().timespan_seconds, 40)
  assert_eq(burst_download_anomaly.unwrap().severity, "critical")
  
  // 验证异常资源访问检测
  let unusual_resource_anomaly = access_anomalies.find_fn(a) { a.anomaly_type == "unusual_resource_access" }
  assert_true(unusual_resource_anomaly.is_some())
  assert_eq(unusual_resource_anomaly.unwrap().user_id, "user004")
  assert_eq(unusual_resource_anomaly.unwrap().resource_types.length(), 1)
  assert_true(unusual_resource_anomaly.unwrap().resource_types.contains("source_code"))
  assert_eq(unusual_resource_anomaly.unwrap().severity, "critical")
  
  // 验证数据外泄模式检测
  assert_eq(exfiltration_indicators.length(), 4)
  
  // 验证单IP大量下载检测
  let large_volume_ip = exfiltration_indicators.find_fn(i) { i.indicator_type == "large_volume_from_single_ip" }
  assert_true(large_volume_ip.is_some())
  assert_eq(large_volume_ip.unwrap().ip_address, "198.51.100.22")
  assert_eq(large_volume_ip.unwrap().total_bytes, 31457280)  // 10MB + 20MB
  assert_eq(large_volume_ip.unwrap().severity, "critical")
  
  // 验证外部IP访问敏感数据检测
  let external_access = exfiltration_indicators.find_fn(i) { i.indicator_type == "external_access_to_sensitive_data" }
  assert_true(external_access.is_some())
  assert_eq(external_access.unwrap().ip_address, "203.0.113.45")
  assert_eq(external_access.unwrap().severity, "critical")
  
  // 验证数据泄露风险评分
  assert_eq(risk_scores.length(), 3)
  
  // 验证user004的风险评分（最高）
  let user004_risk = risk_scores[0]
  assert_eq(user004_risk.user_id, "user004")
  assert_eq(user004_risk.risk_score, 80)  // 30 (large_data_download) + 50 (unusual_resource_access)
  assert_eq(user004_risk.risk_level, "critical")
  assert_eq(user004_risk.risk_factors.length(), 2)
  
  // 验证user003的风险评分
  let user003_risk = risk_scores[1]
  assert_eq(user003_risk.user_id, "user003")
  assert_eq(user003_risk.risk_score, 60)  // 20 (multi_location) + 40 (burst_downloads)
  assert_eq(user003_risk.risk_level, "high")
  assert_eq(user003_risk.risk_factors.length(), 2)
}

// 测试3: 内部威胁检测
test "内部威胁检测" {
  // 模拟内部员工行为数据
  let employee_activities = [
    { 
      timestamp: 1640995200, 
      employee_id: "emp001", 
      department: "engineering", 
      activity_type: "login", 
      system: "dev_server", 
      ip_address: "192.168.1.100",
      location: "office"
    },
    { 
      timestamp: 1640995210, 
      employee_id: "emp001", 
      department: "engineering", 
      activity_type: "file_access", 
      system: "dev_server", 
      file_path: "/src/auth_service.py",
      ip_address: "192.168.1.100",
      location: "office"
    },
    { 
      timestamp: 1640995220, 
      employee_id: "emp001", 
      department: "engineering", 
      activity_type: "file_download", 
      system: "dev_server", 
      file_path: "/src/auth_service.py",
      ip_address: "192.168.1.100",
      location: "office"
    },
    { 
      timestamp: 1640995230, 
      employee_id: "emp002", 
      department: "hr", 
      activity_type: "login", 
      system: "hr_system", 
      ip_address: "192.168.1.101",
      location: "office"
    },
    { 
      timestamp: 1640995240, 
      employee_id: "emp002", 
      department: "hr", 
      activity_type: "data_export", 
      system: "hr_system", 
      data_type: "employee_records",
      record_count: 150,
      ip_address: "192.168.1.101",
      location: "office"
    },
    { 
      timestamp: 1640995250, 
      employee_id: "emp002", 
      department: "hr", 
      activity_type: "email_send", 
      system: "email", 
      recipient: "personal@email.com",
      attachment_size: 2048576,  // 2MB
      ip_address: "192.168.1.101",
      location: "office"
    },
    { 
      timestamp: 1640995260, 
      employee_id: "emp003", 
      department: "finance", 
      activity_type: "login", 
      system: "finance_system", 
      ip_address: "203.0.113.45",
      location: "remote"
    },
    { 
      timestamp: 1640995270, 
      employee_id: "emp003", 
      department: "finance", 
      activity_type: "login", 
      system: "hr_system", 
      ip_address: "203.0.113.45",
      location: "remote"
    },
    { 
      timestamp: 1640995280, 
      employee_id: "emp003", 
      department: "finance", 
      activity_type: "data_access", 
      system: "hr_system", 
      data_type: "salary_records",
      record_count: 200,
      ip_address: "203.0.113.45",
      location: "remote"
    },
    { 
      timestamp: 1640995290, 
      employee_id: "emp003", 
      department: "finance", 
      activity_type: "file_download", 
      system: "hr_system", 
      file_path: "/exports/salary_data.xlsx",
      ip_address: "203.0.113.45",
      location: "remote"
    }
  ]
  
  // 检测异常访问模式
  let detect_abnormal_access_patterns = fn(activities: Array[EmployeeActivity>) {
    let mut threats = []
    
    // 按员工分组
    let employee_groups = activities.group_by(fn(a) { a.employee_id })
    
    for (employee_id, employee_activities) in employee_groups {
      // 获取员工部门
      let department = employee_activities[0].department
      
      // 检测跨部门系统访问
      let accessed_systems = employee_activities.map_fn(a) { a.system }.unique()
      let department_systems = [
        ("engineering", ["dev_server", "git_repo", "jenkins"]),
        ("hr", ["hr_system", "recruitment_portal"]),
        ("finance", ["finance_system", "accounting_software"])
      ]
      
      let allowed_systems = 
        if department == "engineering" { ["dev_server", "git_repo", "jenkins"] }
        else if department == "hr" { ["hr_system", "recruitment_portal"] }
        else if department == "finance" { ["finance_system", "accounting_software"] }
        else { [] }
      
      let unauthorized_systems = accessed_systems.filter_fn(system) { 
        not allowed_systems.contains(system)
      }
      
      if unauthorized_systems.length() > 0 {
        threats = threats.push({
          employee_id: employee_id,
          threat_type: "unauthorized_system_access",
          department: department,
          accessed_systems: accessed_systems,
          unauthorized_systems: unauthorized_systems,
          severity: "high"
        })
      }
      
      // 检测异常时间访问
      let login_activities = employee_activities.filter_fn(a) { a.activity_type == "login" }
      let after_hours_logins = login_activities.filter_fn(a) { 
        // 简化：假设所有访问都是异常时间（实际中应该检查具体时间）
        a.location == "remote"
      }
      
      if after_hours_logins.length() > 0 {
        threats = threats.push({
          employee_id: employee_id,
          threat_type: "after_hours_access",
          department: department,
          login_count: after_hours_logins.length(),
          locations: after_hours_logins.map_fn(a) { a.location }.unique(),
          severity: "medium"
        })
      }
      
      // 检测大量数据访问
      let data_exports = employee_activities.filter_fn(a) { 
        a.activity_type == "data_export" or a.activity_type == "data_access"
      }
      
      let total_records_accessed = data_exports.map_fn(a) { 
        if a.has_field("record_count") { a.record_count } else { 0 }
      }.sum()
      
      if total_records_accessed > 100 {
        threats = threats.push({
          employee_id: employee_id,
          threat_type: "large_data_access",
          department: department,
          total_records: total_records_accessed,
          data_types: data_exports.map_fn(a) { 
            if a.has_field("data_type") { a.data_type } else { "unknown" }
          }.unique(),
          severity: "high"
        })
      }
      
      // 检测数据外泄行为
      let email_sends = employee_activities.filter_fn(a) { a.activity_type == "email_send" }
      let file_downloads = employee_activities.filter_fn(a) { a.activity_type == "file_download" }
      
      if email_sends.length() > 0 or file_downloads.length() > 0 {
        let external_indicators = []
        
        for email in email_sends {
          if email.has_field("recipient") and email.recipient.contains("@") {
            let domain = email.recipient.split("@")[1]
            if domain != "company.com" {
              external_indicators = external_indicators.push({
                type: "external_email",
                details: email.recipient
              })
            }
          }
        }
        
        if external_indicators.length() > 0 {
          threats = threats.push({
            employee_id: employee_id,
            threat_type: "potential_data_exfiltration",
            department: department,
            indicators: external_indicators,
            severity: "critical"
          })
        }
      }
    }
    
    threats
  }
  
  // 计算内部威胁风险评分
  let calculate_insider_threat_risk = fn(threats: Array[InsiderThreat>) {
    let mut employee_risks = []
    
    // 按员工分组威胁
    let employee_threats = threats.group_by(fn(t) { t.employee_id })
    
    for (employee_id, emp_threats) in employee_threats {
      let mut risk_score = 0
      let mut risk_factors = []
      
      for threat in emp_threats {
        match threat.threat_type {
          "unauthorized_system_access" => {
            risk_score = risk_score + 40
            risk_factors = risk_factors.push("unauthorized_system_access")
          }
          "after_hours_access" => {
            risk_score = risk_score + 20
            risk_factors = risk_factors.push("after_hours_access")
          }
          "large_data_access" => {
            risk_score = risk_score + 30
            risk_factors = risk_factors.push("large_data_access")
          }
          "potential_data_exfiltration" => {
            risk_score = risk_score + 50
            risk_factors = risk_factors.push("potential_data_exfiltration")
          }
          _ => {}
        }
      }
      
      let risk_level = 
        if risk_score >= 80 { "critical" }
        else if risk_score >= 60 { "high" }
        else if risk_score >= 30 { "medium" }
        else if risk_score > 0 { "low" }
        else { "minimal" }
      
      employee_risks = employee_risks.push({
        employee_id: employee_id,
        department: emp_threats[0].department,
        risk_score: risk_score,
        risk_level: risk_level,
        threat_count: emp_threats.length(),
        risk_factors: risk_factors
      })
    }
    
    // 按风险评分排序
    employee_risks.sort_by(fn(a, b) { b.risk_score - a.risk_score })
  }
  
  // 生成内部威胁缓解建议
  let generate_insider_threat_mitigation = fn(threats: Array[InsiderThreat>) {
    let mut recommendations = []
    
    // 按威胁类型分组
    let threat_types = threats.map_fn(t) { t.threat_type }.unique()
    
    for threat_type in threat_types {
      let type_threats = threats.filter_fn(t) { t.threat_type == threat_type }
      
      match threat_type {
        "unauthorized_system_access" => {
          recommendations = recommendations.push({
            threat_type: threat_type,
            recommendation: "Implement stricter access controls and role-based permissions",
            affected_employees: type_threats.map_fn(t) { t.employee_id },
            priority: "high"
          })
        }
        "after_hours_access" => {
          recommendations = recommendations.push({
            threat_type: threat_type,
            recommendation: "Review and approve after-hours access requests, implement time-based restrictions",
            affected_employees: type_threats.map_fn(t) { t.employee_id },
            priority: "medium"
          })
        }
        "large_data_access" => {
          recommendations = recommendations.push({
            threat_type: threat_type,
            recommendation: "Implement data access quotas and review large data export requests",
            affected_employees: type_threats.map_fn(t) { t.employee_id },
            priority: "high"
          })
        }
        "potential_data_exfiltration" => {
          recommendations = recommendations.push({
            threat_type: threat_type,
            recommendation: "Implement DLP controls, monitor external communications, and conduct immediate investigation",
            affected_employees: type_threats.map_fn(t) { t.employee_id },
            priority: "critical"
          })
        }
        _ => {}
      }
    }
    
    // 按优先级排序
    let priority_order = ["critical", "high", "medium", "low"]
    recommendations.sort_by(fn(a, b) { 
      let a_priority_index = priority_order.index_of(a.priority).unwrap_or(999)
      let b_priority_index = priority_order.index_of(b.priority).unwrap_or(999)
      a_priority_index - b_priority_index
    })
  }
  
  // 执行内部威胁检测
  let insider_threats = detect_abnormal_access_patterns(employee_activities)
  let threat_risks = calculate_insider_threat_risk(insider_threats)
  let mitigation_recommendations = generate_insider_threat_mitigation(insider_threats)
  
  // 验证异常访问模式检测
  assert_eq(insider_threats.length(), 5)
  
  // 验证未授权系统访问检测
  let unauthorized_access = insider_threats.filter_fn(t) { t.threat_type == "unauthorized_system_access" }
  assert_eq(unauthorized_access.length(), 1)
  assert_eq(unauthorized_access[0].employee_id, "emp003")
  assert_eq(unauthorized_access[0].department, "finance")
  assert_true(unauthorized_access[0].unauthorized_systems.contains("hr_system"))
  assert_eq(unauthorized_access[0].severity, "high")
  
  // 验证异常时间访问检测
  let after_hours_access = insider_threats.filter_fn(t) { t.threat_type == "after_hours_access" }
  assert_eq(after_hours_access.length(), 1)
  assert_eq(after_hours_access[0].employee_id, "emp003")
  assert_eq(after_hours_access[0].department, "finance")
  assert_eq(after_hours_access[0].severity, "medium")
  
  // 验证大量数据访问检测
  let large_data_access = insider_threats.filter_fn(t) { t.threat_type == "large_data_access" }
  assert_eq(large_data_access.length(), 2)
  
  let emp002_data_access = large_data_access.find_fn(t) { t.employee_id == "emp002" }
  assert_true(emp002_data_access.is_some())
  assert_eq(emp002_data_access.unwrap().total_records, 150)
  assert_eq(emp002_data_access.unwrap().severity, "high")
  
  let emp003_data_access = large_data_access.find_fn(t) { t.employee_id == "emp003" }
  assert_true(emp003_data_access.is_some())
  assert_eq(emp003_data_access.unwrap().total_records, 200)
  assert_eq(emp003_data_access.unwrap().severity, "high")
  
  // 验证潜在数据外泄检测
  let data_exfiltration = insider_threats.filter_fn(t) { t.threat_type == "potential_data_exfiltration" }
  assert_eq(data_exfiltration.length(), 1)
  assert_eq(data_exfiltration[0].employee_id, "emp002")
  assert_eq(data_exfiltration[0].severity, "critical")
  
  // 验证内部威胁风险评分
  assert_eq(threat_risks.length(), 3)
  
  // 验证emp003的风险评分（最高）
  let emp003_risk = threat_risks[0]
  assert_eq(emp003_risk.employee_id, "emp003")
  assert_eq(emp003_risk.department, "finance")
  assert_eq(emp003_risk.risk_score, 90)  // 40 + 20 + 30
  assert_eq(emp003_risk.risk_level, "critical")
  assert_eq(emp003_risk.threat_count, 3)
  
  // 验证emp002的风险评分
  let emp002_risk = threat_risks[1]
  assert_eq(emp002_risk.employee_id, "emp002")
  assert_eq(emp002_risk.department, "hr")
  assert_eq(emp002_risk.risk_score, 80)  // 30 + 50
  assert_eq(emp002_risk.risk_level, "critical")
  assert_eq(emp002_risk.threat_count, 2)
  
  // 验证缓解建议
  assert_eq(mitigation_recommendations.length(), 4)
  
  // 验证最高优先级建议
  let top_priority = mitigation_recommendations[0]
  assert_eq(top_priority.threat_type, "potential_data_exfiltration")
  assert_eq(top_priority.priority, "critical")
  assert_true(top_priority.affected_employees.contains("emp002"))
}