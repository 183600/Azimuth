// Azimuth 新增测试套件基础文件
// 提供基本的类型和函数定义

// 基础遥测数据类型
pub type TelemetryData {
  id : Int
  metric : String
  value : Double
  timestamp : Int
}

// 基础服务数据类型
pub type ServiceData {
  service : String
  metric : String
  value : Double
  timestamp : Int
}

// 基础指标数据类型
pub type MetricData {
  name : String
  value : Double
  unit : String
  timestamp : Int
}