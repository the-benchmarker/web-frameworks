extension String: Error { }

extension Array where Element == Int {
    /// Returns the average of all elements in the array
    var average: Int {
        switch count {
        case 0: return 0
        case 1: return self[0]
        default:
            var total = 0
            for el in self {
                total += el
            }
            return total / count
        }
    }
}
