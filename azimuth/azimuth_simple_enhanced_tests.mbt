// Azimuth 项目简单增强测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "azimuth_basic_arithmetic" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(21, multiply(3, 7))
  assert_eq(5, subtract(12, 7))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "azimuth_mathematical_properties" {
  // 数学性质验证测试
  let a = 15
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "azimuth_navigation_calculations" {
  // 导航计算测试
  let base_angle = 45
  let total_rotations = 8
  
  // 计算总角度
  let total_angle = multiply(base_angle, total_rotations)
  assert_eq(360, total_angle)
  
  // 计算分段角度
  let segment_angle = divide_with_ceil(total_angle, 30)
  assert_eq(12, segment_angle)
}

test "azimuth_coordinate_system" {
  // 坐标系统测试
  let x_coordinate = 128
  let grid_size = 64
  
  // 计算网格位置
  let grid_x = divide_with_ceil(x_coordinate, grid_size)
  assert_eq(2, grid_x)
  
  // 验证总容量
  let total_capacity = multiply(grid_x, grid_size)
  assert_eq(128, total_capacity)
}

test "azimuth_path_planning" {
  // 路径规划测试
  let start_x = 0
  let end_x = 150
  let step_size = 25
  
  // 计算总距离
  let delta_x = subtract(end_x, start_x)
  assert_eq(150, delta_x)
  
  // 计算步数
  let steps_needed = divide_with_ceil(delta_x, step_size)
  assert_eq(6, steps_needed)
}

test "azimuth_sensor_processing" {
  // 传感器数据处理测试
  let reading_count = 5
  
  // 计算平均值 (简化)
  let sum_readings = add(add(add(add(12, 15), 18), 21), 24)
  let average_reading = divide_with_ceil(sum_readings, reading_count)
  
  assert_eq(90, sum_readings)
  assert_eq(18, average_reading)
}

test "azimuth_error_correction" {
  // 错误纠正测试
  let measured_value = 95
  let expected_value = 100
  
  // 计算误差
  let error_value = subtract(measured_value, expected_value)
  assert_eq(-5, error_value)
  
  // 计算绝对误差
  let absolute_error = if error_value < 0 { multiply(error_value, -1) } else { error_value }
  assert_eq(5, absolute_error)
}

test "azimuth_greeting_function" {
  // 问候函数测试
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
}