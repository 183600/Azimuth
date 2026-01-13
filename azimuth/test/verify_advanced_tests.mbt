// 验证 advanced_tests.mbt 中的测试用例
// 这个文件用于手动验证新添加的测试用例是否能正常工作

// 导入 azimuth 包
import "azimuth"

// 手动运行 advanced_tests.mbt 中的测试用例
fn run_advanced_tests() -> Unit {
  // 测试加法的交换律
  let test1_result1 = @azimuth.add(15, 27)
  let test1_result2 = @azimuth.add(27, 15)
  @azimuth.assert_eq(test1_result1, test1_result2)
  
  let test1_result3 = @azimuth.add(-15, 27)
  let test1_result4 = @azimuth.add(27, -15)
  @azimuth.assert_eq(test1_result3, test1_result4)
  
  let test1_result5 = @azimuth.add(-15, -27)
  let test1_result6 = @azimuth.add(-27, -15)
  @azimuth.assert_eq(test1_result5, test1_result6)
  
  // 测试加法的结合律
  let test2_result1 = @azimuth.add(@azimuth.add(10, 20), 30)
  let test2_result2 = @azimuth.add(10, @azimuth.add(20, 30))
  @azimuth.assert_eq(test2_result1, test2_result2)
  
  let test2_result3 = @azimuth.add(@azimuth.add(-10, 20), 30)
  let test2_result4 = @azimuth.add(-10, @azimuth.add(20, 30))
  @azimuth.assert_eq(test2_result3, test2_result4)
  
  // 测试乘法的交换律
  let test3_result1 = @azimuth.multiply(12, 8)
  let test3_result2 = @azimuth.multiply(8, 12)
  @azimuth.assert_eq(test3_result1, test3_result2)
  
  let test3_result3 = @azimuth.multiply(-12, 8)
  let test3_result4 = @azimuth.multiply(8, -12)
  @azimuth.assert_eq(test3_result3, test3_result4)
  
  let test3_result5 = @azimuth.multiply(-12, -8)
  let test3_result6 = @azimuth.multiply(-8, -12)
  @azimuth.assert_eq(test3_result5, test3_result6)
  
  // 测试乘法的结合律
  let test4_result1 = @azimuth.multiply(@azimuth.multiply(3, 4), 5)
  let test4_result2 = @azimuth.multiply(3, @azimuth.multiply(4, 5))
  @azimuth.assert_eq(test4_result1, test4_result2)
  
  let test4_result3 = @azimuth.multiply(@azimuth.multiply(-3, 4), 5)
  let test4_result4 = @azimuth.multiply(-3, @azimuth.multiply(4, 5))
  @azimuth.assert_eq(test4_result3, test4_result4)
  
  // 测试乘法对加法的分配律
  let test5_result1 = @azimuth.multiply(3, @azimuth.add(4, 5))
  let test5_result2 = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.multiply(3, 5))
  @azimuth.assert_eq(test5_result1, test5_result2)
  
  let test5_result3 = @azimuth.multiply(-3, @azimuth.add(4, 5))
  let test5_result4 = @azimuth.add(@azimuth.multiply(-3, 4), @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(test5_result3, test5_result4)
  
  // 测试大数运算
  let large_num = 1000000
  let test6_result1 = @azimuth.add(large_num, large_num)
  @azimuth.assert_eq(2000000, test6_result1)
  
  let test6_result2 = @azimuth.multiply(large_num, large_num)
  @azimuth.assert_eq(1000000000000, test6_result2)
  
  // 测试混合运算场景
  let step1 = @azimuth.add(2, 3)
  let step2 = @azimuth.multiply(step1, 4)
  let test7_result = @azimuth.add(step2, -5)
  @azimuth.assert_eq(15, test7_result)
  
  // 测试字符串拼接的复杂场景
  let long_name = "VeryLongNameThatContainsManyCharactersAndNumbers123456789"
  let test8_result1 = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, VeryLongNameThatContainsManyCharactersAndNumbers123456789!", test8_result1)
  
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let test8_result2 = @azimuth.greet(special_chars)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", test8_result2)
  
  // 测试边界值的组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  let step1 = @azimuth.add(max_val, min_val)
  let test9_result = @azimuth.add(step1, 1)
  @azimuth.assert_eq(0, test9_result)
  
  let test9_result2 = @azimuth.multiply(min_val, 0)
  @azimuth.assert_eq(0, test9_result2)
  
  let test9_result3 = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, test9_result3)
  
  // 测试实际应用场景
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(612, total)
  
  let username = "张三"
  let welcome_msg = @azimuth.greet(username)
  @azimuth.assert_eq_string("Hello, 张三!", welcome_msg)
  
  // 所有测试通过
  @azimuth.assert_true(true)
}

// 运行测试
run_advanced_tests()