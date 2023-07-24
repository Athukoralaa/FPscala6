object CaesarCipher {

  //Encryption function
  def caesarEncrypt(plainText: String, shift: Int): String = {
    def encryptRecursively(remainingText: String, encryptedText: String): String = {
      if (remainingText.isEmpty) encryptedText
      else {
        val char = remainingText.head
        val encryptedChar = char match {
          case c if c.isUpper => ((c - 'A' + shift) % 26 + 'A').toChar
          case c if c.isLower => ((c - 'a' + shift) % 26 + 'a').toChar
          case _ => char
        }
        encryptRecursively(remainingText.tail, encryptedText + encryptedChar)
      }
    }

    encryptRecursively(plainText, "")
  }

  // Decryption function
  def caesarDecrypt(encryptedText: String, shift: Int): String = {
    caesarEncrypt(encryptedText, 26 - shift)
  }

  // Cipher function
  def cipher(text: String, action: String, shift: Int): String = {
    action.toLowerCase match {
      case "encrypt" => caesarEncrypt(text, shift)
      case "decrypt" => caesarDecrypt(text, shift)
      case _ => throw new IllegalArgumentException("Invalid action. Use 'encrypt' or 'decrypt'.")
    }
  }

  def main(args: Array[String]): Unit = {
    val textToEncrypt = "I love UCSC!"
    val shiftAmount = 1

    
    val encryptedText = cipher(textToEncrypt, "encrypt", shiftAmount)
    println(s"Encrypted: $encryptedText") 


    val decryptedText = cipher(encryptedText, "decrypt", shiftAmount)
    println(s"Decrypted: $decryptedText") 
  }
}
