// 新增的 MoonBit 测试用例
// 测试 azimuth 包中的核心功能

test "arithmetics_edge_cases" {
  // 测试加法的边缘情况
  @azimuth.@azimuth.assert_eq(2147483647, @azimuth.@azimuth.add(2147483646, 1))
  @azimuth.@azimuth.assert_eq(-2147483648, @azimuth.@azimuth.add(-2147483647, -1))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.add(-1000, 1000))
}

test "multiply_boundary_tests" {
  // 测试乘法精度和边界情况
  @azimuth.@azimuth.assert_eq(2147483647, @azimuth.@azimuth.multiply(46340, 46340))
  @azimuth.@azimuth.assert_eq(-2147483648, @azimuth.@azimuth.multiply(-46341, 46341))
  @azimuth.@azimuth.assert_eq(0, @azimuth.@azimuth.multiply(12345, 0))
}

test "string_internationalization" {
  // 测试字符串问候的国际化支持
  @azimuth.@azimuth.assert_eq_string("Hello, 世界!", @azimuth.@azimuth.greet("世界"))
  @azimuth.@azimuth.assert_eq_string("Hello, Москва!", @azimuth.@azimuth.greet("Москва"))
  @azimuth.@azimuth.assert_eq_string("Hello, 🚀!", @azimuth.@azimuth.greet("🚀"))
}

test "fibonacci_sequence" {
  // 测试斐波那契数列第10项
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.@azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.@azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.@azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.@azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.@azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.@azimuth.add(fib6, fib7)  // 21
  let fib9 = @azimuth.@azimuth.add(fib7, fib8)  // 34
  let fib10 = @azimuth.@azimuth.add(fib8, fib9) // 55
  
  @azimuth.@azimuth.assert_eq(55, fib10)
}

test "geometry_calculations" {
  // 测试几何计算
  // 计算三角形面积（简化版）
  let a = 3
  let b = 4
  
  // 简化面积计算：直角三角形面积 = 底 × 高 / 2
  let area = @azimuth.@azimuth.multiply(a, b) / 2
  @azimuth.@azimuth.assert_eq(6, area)
  
  // 计算立方体体积和表面积
  let side = 5
  let volume = @azimuth.@azimuth.multiply(side, @azimuth.@azimuth.multiply(side, side))
  let surface_area = @azimuth.@azimuth.multiply(6, @azimuth.@azimuth.multiply(side, side))
  
  @azimuth.@azimuth.assert_eq(125, volume)
  @azimuth.@azimuth.assert_eq(150, surface_area)
}