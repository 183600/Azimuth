// Azimuth 项目导航与方位角计算测试用例
// 使用标准 MoonBit 测试语法，专注于导航和方位角相关功能

// 导入必要的模块
use azimuth

test "basic_azimuth_calculation" {
  // 基本方位角计算测试
  // 方位角是从正北方向顺时针测量的角度（0-360度）
  
  let north = 0      // 正北
  let east = 90       // 正东
  let south = 180     // 正南
  let west = 270      // 正西
  let north_east = 45 // 东北
  
  // 验证基本方向
  @assertion.assert_eq(0, north)?
  @assertion.assert_eq(90, east)?
  @assertion.assert_eq(180, south)?
  @assertion.assert_eq(270, west)?
  @assertion.assert_eq(45, north_east)?
}

test "bearing_conversion" {
  // 方位角转换测试
  // 将度数转换为方位角表示
  
  let degrees = 135  // 东南方向
  let bearing = azimuth.to_bearing(degrees)
  @assertion.assert_eq_string("SE", bearing)?
  
  // 测试其他方向
  let north_bearing = azimuth.to_bearing(0)
  @assertion.assert_eq_string("N", north_bearing)?
  
  let east_bearing = azimuth.to_bearing(90)
  @assertion.assert_eq_string("E", east_bearing)?
  
  let south_bearing = azimuth.to_bearing(180)
  @assertion.assert_eq_string("S", south_bearing)?
  
  let west_bearing = azimuth.to_bearing(270)
  @assertion.assert_eq_string("W", west_bearing)?
}

test "angle_normalization" {
  // 角度标准化测试
  // 将任意角度标准化到 0-360 度范围
  
  // 测试超出范围的角度
  let angle1 = azimuth.normalize_angle(370)  // 应该变成 10 度
  @assertion.assert_eq(10, angle1)?
  
  let angle2 = azimuth.normalize_angle(-30)  // 应该变成 330 度
  @assertion.assert_eq(330, angle2)?
  
  let angle3 = azimuth.normalize_angle(720)  // 应该变成 0 度
  @assertion.assert_eq(0, angle3)?
  
  let angle4 = azimuth.normalize_angle(450)  // 应该变成 90 度
  @assertion.assert_eq(90, angle4)?
}

test "compass_point_calculation" {
  // 罗盘点计算测试
  // 计算两点之间的方位角
  
  let lat1 = 40.7128  // 纽约纬度
  let lon1 = -74.0060 // 纽约经度
  let lat2 = 51.5074  // 伦敦纬度
  let lon2 = -0.1278  // 伦敦经度
  
  // 简化的方位角计算（实际应该使用球面三角学）
  let delta_lon = azimuth.add(lon2, -lon1)
  let bearing = azimuth.add(azimuth.multiply(delta_lon, 2), 45) // 简化计算
  let normalized_bearing = azimuth.normalize_angle(bearing)
  
  // 验证结果在合理范围内
  @assertion.assert_eq(true, normalized_bearing >= 0)?
  @assertion.assert_eq(true, normalized_bearing < 360)?
}

test "distance_calculation" {
  // 距离计算测试
  // 简化的平面距离计算
  
  let x1 = 0
  let y1 = 0
  let x2 = 3
  let y2 = 4
  
  // 计算x和y的差值
  let delta_x = azimuth.add(x2, -x1)
  let delta_y = azimuth.add(y2, -y1)
  
  // 简化的距离计算（曼哈顿距离）
  let manhattan_distance = azimuth.add(delta_x, delta_y)
  @assertion.assert_eq(7, manhattan_distance)?
  
  // 简化的欧几里得距离（不使用平方根）
  let euclidean_squared = azimuth.add(azimuth.multiply(delta_x, delta_x), azimuth.multiply(delta_y, delta_y))
  @assertion.assert_eq(25, euclidean_squared)?
}

test "waypoint_navigation" {
  // 航点导航测试
  // 计算通过多个航点的总距离和方向
  
  // 定义航点坐标
  let waypoint1_x = 0
  let waypoint1_y = 0
  let waypoint2_x = 5
  let waypoint2_y = 0
  let waypoint3_x = 5
  let waypoint3_y = 12
  
  // 计算各段距离
  let leg1_distance = azimuth.add(waypoint2_x, -waypoint1_x)  // 5
  let leg2_distance = azimuth.add(waypoint3_y, -waypoint2_y)  // 12
  
  // 计算总距离
  let total_distance = azimuth.add(leg1_distance, leg2_distance)
  @assertion.assert_eq(17, total_distance)?
  
  // 计算总体方向（简化）
  let overall_x = azimuth.add(waypoint3_x, -waypoint1_x)
  let overall_y = azimuth.add(waypoint3_y, -waypoint1_y)
  let overall_direction = azimuth.normalize_angle(azimuth.add(azimuth.multiply(overall_x, 10), overall_y))
  
  // 验证方向在合理范围内
  @assertion.assert_eq(true, overall_direction >= 0)?
  @assertion.assert_eq(true, overall_direction < 360)?
}

test "coordinate_conversion" {
  // 坐标转换测试
  // 十进制度数与度分秒之间的转换
  
  // 十进制度数转度分秒
  let decimal_degrees = 40.7128
  
  let degrees = azimuth.to_int(decimal_degrees)  // 40
  let decimal_minutes = azimuth.add(decimal_degrees, -degrees)  // 0.7128
  let minutes = azimuth.to_int(azimuth.multiply(decimal_minutes, 60))  // 42
  let decimal_seconds = azimuth.add(azimuth.multiply(decimal_minutes, 60), -minutes)  // 0.768
  let seconds = azimuth.to_int(azimuth.multiply(decimal_seconds, 60))  // 46
  
  @assertion.assert_eq(40, degrees)?
  @assertion.assert_eq(42, minutes)?
  @assertion.assert_eq(46, seconds)?
}

test "magnetic_declination" {
  // 磁偏角计算测试
  // 真北与磁北之间的角度差
  
  let true_bearing = 90  // 真方位角（正东）
  let magnetic_declination = 15  // 磁偏角
  
  // 计算磁方位角
  let magnetic_bearing = azimuth.add(true_bearing, -magnetic_declination)
  let normalized_magnetic = azimuth.normalize_angle(magnetic_bearing)
  @assertion.assert_eq(75, normalized_magnetic)?
  
  // 反向计算：从磁方位角计算真方位角
  let reverse_true = azimuth.add(normalized_magnetic, magnetic_declination)
  let reverse_normalized = azimuth.normalize_angle(reverse_true)
  @assertion.assert_eq(90, reverse_normalized)?
}

test "navigation_triangle" {
  // 导航三角形计算测试
  // 使用简化的平面三角形计算
  
  let side_a = 3  // 三角形边长
  let side_b = 4
  let side_c = 5
  
  // 验证勾股定理
  let pythagoras_check = azimuth.add(azimuth.multiply(side_a, side_a), azimuth.multiply(side_b, side_b))
  @assertion.assert_eq(azimuth.multiply(side_c, side_c), pythagoras_check)?
  
  // 计算周长
  let perimeter = azimuth.add(azimuth.add(side_a, side_b), side_c)
  @assertion.assert_eq(12, perimeter)?
  
  // 计算角度（简化计算）
  let angle_a = azimuth.multiply(side_a, 20)  // 简化的角度计算
  let angle_b = azimuth.multiply(side_b, 20)
  let angle_c = azimuth.multiply(side_c, 20)
  
  let angle_sum = azimuth.add(azimuth.add(angle_a, angle_b), angle_c)
  @assertion.assert_eq(240, angle_sum)?  // 简化计算的总和
}

test "course_correction" {
  // 航向修正测试
  // 根据当前位置和目标位置计算修正角度
  
  let current_heading = 45  // 当前航向（东北）
  let target_heading = 90   // 目标航向（正东）
  
  // 计算需要修正的角度
  let correction_needed = azimuth.add(target_heading, -current_heading)
  let normalized_correction = azimuth.normalize_angle(correction_needed)
  
  // 验证修正角度
  @assertion.assert_eq(45, normalized_correction)?
  
  // 测试需要反向修正的情况
  let reverse_current = 270  // 当前航向（正西）
  let reverse_target = 45    // 目标航向（东北）
  
  let reverse_correction = azimuth.add(reverse_target, -reverse_current)
  let reverse_normalized = azimuth.normalize_angle(reverse_correction)
  
  // 从正西到东北的最短路径应该是135度
  @assertion.assert_eq(135, reverse_normalized)?
}