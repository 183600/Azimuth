// Azimuth 项目增强标准测试用例
// 使用标准 MoonBit 测试语法，包含10个高质量测试用例

// 必要的函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn divide_with_ceil(a : Int, b : Int) -> Int {
  if b == 0 { 
    0 
  } else {
    let quotient = a / b
    let remainder = a % b
    if remainder == 0 {
      quotient
    } else if a > 0 && b > 0 {
      quotient + 1
    } else if a < 0 && b < 0 {
      quotient + 1
    } else {
      quotient
    }
  }
}

test "arithmetic_sequence_calculation" {
  // 算术序列计算：计算等差数列前n项和
  let first_term = 3
  let common_diff = 2
  let num_terms = 10
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  ) = 120  // 10/2 * (6 + 18) = 5 * 24 = 120
}

test "resource_allocation_optimization" {
  // 资源分配优化：计算项目资源需求
  let total_tasks = 87
  let team_members = 8
  let working_days = 5
  
  // 计算每人每天需要完成的任务数
  divide_with_ceil(total_tasks, multiply(team_members, working_days)) = 3  // ceil(87/(8*5)) = 3
  
  // 计算总工作日需求
  divide_with_ceil(total_tasks, multiply(team_members, 3)) = 4  // ceil(87/(8*3)) = 4
}

test "financial_compound_interest" {
  // 金融复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 简化的年复利计算
  add(principal, divide_with_ceil(multiply(principal, rate_percent), 100)) = 1050  // 1000 + 5%
  add(1050, divide_with_ceil(multiply(1050, rate_percent), 100)) = 1103  // 1050 + 5%
  add(1103, divide_with_ceil(multiply(1103, rate_percent), 100)) = 1158  // 1103 + 5%
}

test "logistics_packaging_optimization" {
  // 物流包装优化：计算最经济的包装方案
  let items_a = 25  // 产品A数量
  let items_b = 47  // 产品B数量
  let capacity_a = 8   // A产品每箱容量
  let capacity_b = 12  // B产品每箱容量
  
  divide_with_ceil(items_a, capacity_a) = 4    // ceil(25/8) = 4
  divide_with_ceil(items_b, capacity_b) = 4    // ceil(47/12) = 4
  add(4, 4) = 8  // 4 + 4 = 8
}

test "temperature_conversion_analysis" {
  // 温度转换分析：批量温度数据处理
  let celsius_temps_sum = add(add(add(add(0, 10), 20), 30), 40)
  let temp_count = 5
  
  // 计算平均摄氏温度
  divide_with_ceil(celsius_temps_sum, temp_count) = 20  // (0+10+20+30+40)/5 = 20
  
  // 转换为华氏度：F = C * 9/5 + 32
  add(divide_with_ceil(multiply(20, 9), 5), 32) = 68  // 20*9/5 + 32 = 36 + 32 = 68
}

test "algorithm_complexity_comparison" {
  // 算法复杂度比较：模拟不同算法的性能特征
  let input_size = 1000
  
  // O(n) 线性算法
  multiply(input_size, 3) = 3000  // 1000 * 3
  
  // O(log n) 对数算法（简化模拟）
  divide_with_ceil(input_size, 100) = 10  // ceil(1000/100)
  
  // O(n²) 平方算法（简化模拟）
  divide_with_ceil(multiply(input_size, input_size), 1000) = 1000  // ceil(1000*1000/1000)
}

test "inventory_turnover_calculation" {
  // 库存周转率计算
  let beginning_inventory = 500
  let ending_inventory = 300
  let cost_of_goods_sold = 1200
  
  // 计算平均库存
  divide_with_ceil(add(beginning_inventory, ending_inventory), 2) = 400  // (500 + 300)/2 = 400
  
  // 计算库存周转率
  divide_with_ceil(multiply(cost_of_goods_sold, 100), 400) = 300  // (1200 * 100)/400 = 300
}

test "network_data_transfer_calculation" {
  // 网络数据传输计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  // 计算基础传输时间
  divide_with_ceil(file_size_mb, network_speed_mbps) = 17  // ceil(850/50) = 17
  
  // 计算开销时间
  divide_with_ceil(multiply(17, overhead_percentage), 100) = 2  // ceil(17*10/100) = 2
  
  // 计算总传输时间
  add(17, 2) = 19  // 17 + 2 = 19
}

test "energy_efficiency_analysis" {
  // 能效分析：计算设备能效比
  let power_input_watts = 1000
  let useful_output_watts = 850
  let operating_hours = 24
  
  // 计算能效百分比
  divide_with_ceil(multiply(useful_output_watts, 100), power_input_watts) = 85  // (850 * 100)/1000 = 85
  
  // 计算日能耗
  multiply(power_input_watts, operating_hours) = 24000  // 1000 * 24
  
  // 计算有效能耗
  multiply(useful_output_watts, operating_hours) = 20400  // 850 * 24
}

test "project_management_metrics" {
  // 项目管理指标计算
  let planned_hours = 160
  let actual_hours = 180
  let team_size = 8
  let project_duration_days = 20
  
  // 计算效率指数
  divide_with_ceil(multiply(planned_hours, 100), actual_hours) = 88  // (160 * 100)/180 = 88
  
  // 计算每人每天平均工作小时数
  divide_with_ceil(actual_hours, multiply(team_size, project_duration_days)) = 2  // ceil(180/(8*20)) = 2
  
  // 计算时间偏差百分比
  divide_with_ceil(multiply(add(actual_hours, -planned_hours), 100), planned_hours) = 13  // ((180-160) * 100)/160 = 13
}