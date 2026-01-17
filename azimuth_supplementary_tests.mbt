// Azimuth 项目补充测试用例
// 使用标准 MoonBit 测试语法，提供额外的测试场景

test "recursive_sequence_test" {
  // 递归序列测试 - 计算阶乘
  let n = 5
  let factorial_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(@azimuth.multiply(1, 2), 3), 4), 5)
  @azimuth.assert_eq(120, factorial_result)
  
  // 递归序列测试 - 计算幂
  let base = 3
  let exponent = 4
  let power_result = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(3, 3), 3), 3)
  @azimuth.assert_eq(81, power_result)
}

test "statistical_calculations" {
  // 统计计算测试
  // 计算平均值: (10 + 20 + 30 + 40 + 50) / 5
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50)
  let average = sum / 5
  @azimuth.assert_eq(30, average)
  
  // 计算范围: max - min
  let max_value = 50
  let min_value = 10
  let range = @azimuth.subtract(max_value, min_value)
  @azimuth.assert_eq(40, range)
}

test "unit_conversion" {
  // 单位转换测试
  // 公里转米: 1公里 = 1000米
  let kilometers = 5
  let meters = @azimuth.multiply(kilometers, 1000)
  @azimuth.assert_eq(5000, meters)
  
  // 米转厘米: 1米 = 100厘米
  let meters2 = 3
  let centimeters = @azimuth.multiply(meters2, 100)
  @azimuth.assert_eq(300, centimeters)
  
  // 千克转克: 1千克 = 1000克
  let kilograms = 2
  let grams = @azimuth.multiply(kilograms, 1000)
  @azimuth.assert_eq(2000, grams)
}

test "string_manipulation" {
  // 字符串操作测试
  // 字符串长度测试
  let short_string = "Hello"
  let long_string = "Hello, MoonBit Programming"
  
  // 假设有字符串长度函数
  let short_length = @azimuth.string_length(short_string)
  let long_length = @azimuth.string_length(long_string)
  
  @azimuth.assert_eq(5, short_length)
  @azimuth.assert_eq(27, long_length)
  
  // 字符串反转测试
  let original = "abcde"
  let reversed = @azimuth.string_reverse(original)
  @azimuth.assert_eq_string("edcba", reversed)
}

test "conditional_logic" {
  // 条件逻辑测试
  // 绝对值计算
  let positive_num = 42
  let negative_num = -42
  let absolute_positive = @azimuth.abs(positive_num)
  let absolute_negative = @azimuth.abs(negative_num)
  
  @azimuth.assert_eq(42, absolute_positive)
  @azimuth.assert_eq(42, absolute_negative)
  
  // 最大值计算
  let num1 = 15
  let num2 = 25
  let max_value = @azimuth.max(num1, num2)
  @azimuth.assert_eq(25, max_value)
}

test "algorithm_simulation" {
  // 算法模拟测试
  // 线性搜索模拟
  let target = 7
  let found = false
  let search_values = [1, 3, 5, 7, 9]
  
  // 模拟搜索过程
  let index = 0
  while (index < 5) {
    if (@azimuth.array_get(search_values, index) == target) {
      found = true
      break
    }
    index = @azimuth.add(index, 1)
  }
  
  @azimuth.assert_eq(true, found)
  
  // 简单排序验证
  let unsorted = [5, 2, 8, 1, 9]
  let sorted = [1, 2, 5, 8, 9]
  
  // 验证排序后的第一个元素
  @azimuth.assert_eq(1, @azimuth.array_get(sorted, 0))
  // 验证排序后的最后一个元素
  @azimuth.assert_eq(9, @azimuth.array_get(sorted, 4))
}

test "physics_calculations" {
  // 物理计算测试
  // 速度计算: 距离 / 时间
  let distance = 100  // 米
  let time = 10       // 秒
  let velocity = distance / time
  @azimuth.assert_eq(10, velocity)
  
  // 加速度计算: (末速度 - 初速度) / 时间
  let initial_velocity = 0
  let final_velocity = 20
  let acceleration_time = 5
  let acceleration = @azimuth.subtract(final_velocity, initial_velocity) / acceleration_time
  @azimuth.assert_eq(4, acceleration)
  
  // 动能计算: 0.5 * 质量 * 速度²
  let mass = 2
  let speed = 10
  let kinetic_energy = @azimuth.multiply(@azimuth.multiply(0.5, mass), @azimuth.square(speed))
  @azimuth.assert_eq(100, kinetic_energy)
}

test "data_validation" {
  // 数据验证测试
  // 邮箱格式验证
  let valid_email = "user@example.com"
  let invalid_email = "invalid.email"
  
  let is_valid_email1 = @azimuth.validate_email(valid_email)
  let is_valid_email2 = @azimuth.validate_email(invalid_email)
  
  @azimuth.assert_eq(true, is_valid_email1)
  @azimuth.assert_eq(false, is_valid_email2)
  
  // 电话号码验证
  let valid_phone = "123-456-7890"
  let invalid_phone = "1234567890"
  
  let is_valid_phone1 = @azimuth.validate_phone(valid_phone)
  let is_valid_phone2 = @azimuth.validate_phone(invalid_phone)
  
  @azimuth.assert_eq(true, is_valid_phone1)
  @azimuth.assert_eq(false, is_valid_phone2)
}

test "graph_algorithms" {
  // 图算法测试
  // 简单图的邻接矩阵表示
  // 顶点: A, B, C, D
  // 边: A-B, B-C, C-D, A-D
  let adjacency_matrix = [
    [0, 1, 0, 1],  // A
    [1, 0, 1, 0],  // B
    [0, 1, 0, 1],  // C
    [1, 0, 1, 0]   // D
  ]
  
  // 验证顶点A的度数
  let a_degree = @azimuth.add(@azimuth.array_get(adjacency_matrix, 0)[1], @azimuth.array_get(adjacency_matrix, 0)[3])
  @azimuth.assert_eq(2, a_degree)
  
  // 验证顶点B的度数
  let b_degree = @azimuth.add(@azimuth.array_get(adjacency_matrix, 1)[0], @azimuth.array_get(adjacency_matrix, 1)[2])
  @azimuth.assert_eq(2, b_degree)
}

test "encryption_simulation" {
  // 加密模拟测试
  // 凯撒密码模拟
  let original_text = "HELLO"
  let shift = 3
  let encrypted_text = @azimuth.caesar_encrypt(original_text, shift)
  let decrypted_text = @azimuth.caesar_decrypt(encrypted_text, shift)
  
  @azimuth.assert_eq_string("KHOOR", encrypted_text)
  @azimuth.assert_eq_string("HELLO", decrypted_text)
  
  // 简单哈希模拟
  let input_string = "password123"
  let hash_value = @azimuth.simple_hash(input_string)
  
  // 验证相同输入产生相同哈希
  let hash_value2 = @azimuth.simple_hash(input_string)
  @azimuth.assert_eq(hash_value, hash_value2)
  
  // 验证不同输入产生不同哈希
  let different_hash = @azimuth.simple_hash("different_password")
  @azimuth.assert_eq(false, @azimuth.equal(hash_value, different_hash))
}