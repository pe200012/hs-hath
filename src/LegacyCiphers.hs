-- |
-- Module      : LegacyCiphers
-- License     : BSD-3-Clause
-- Maintainer  : Vincent Hanquez
-- Stability   : experimental
-- Portability : unknown
--
-- This module contains legacy cipher definitions borrowed from the
-- tls package (version 1.9.0) by Vincent Hanquez.
--
-- Original source: https://hackage.haskell.org/package/tls-1.9.0/docs/src/Network.TLS.Extra.Cipher.html
--
-- Copyright (c) Vincent Hanquez
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- 1. Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
-- 3. Neither the name of the copyright holder nor the names of its contributors
--    may be used to endorse or promote products derived from this software without
--    specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- This file is part of the hs-hath project, which is licensed under GPL-3.0.
-- The BSD-3-Clause license applies to this specific file as noted above.
module LegacyCiphers
  ( cipher_ECDHE_ECDSA_AES128CBC_SHA
  , cipher_ECDHE_RSA_AES128CBC_SHA
  , cipher_ECDHE_RSA_AES256CBC_SHA
  ) where

import           Crypto.Cipher.AES   ( AES128, AES256 )
import           Crypto.Cipher.Types ( BlockCipher
                                     , IV
                                     , cbcDecrypt
                                     , cbcEncrypt
                                     , cipherInit
                                     , makeIV
                                     )
import           Crypto.Error        ( CryptoFailable, throwCryptoError )

import qualified Data.ByteString     as B

import           Network.TLS         ( Version(..) )
import           Network.TLS.Cipher  ( Bulk(..)
                                     , BulkBlock
                                     , BulkDirection(..)
                                     , BulkFunctions(..)
                                     , BulkKey
                                     , Cipher(..)
                                     , CipherKeyExchangeType(..)
                                     , Hash(..)
                                     )

import           Relude

cipher_ECDHE_ECDSA_AES128CBC_SHA :: Cipher
cipher_ECDHE_ECDSA_AES128CBC_SHA
  = Cipher { cipherID          = 0xC009
           , cipherName        = "ECDHE-ECDSA-AES128CBC-SHA"
           , cipherBulk        = bulk_aes128
           , cipherHash        = SHA1
           , cipherPRFHash     = Nothing
           , cipherKeyExchange = CipherKeyExchange_ECDHE_ECDSA
           , cipherMinVer      = Just TLS10
           }

cipher_ECDHE_RSA_AES128CBC_SHA :: Cipher
cipher_ECDHE_RSA_AES128CBC_SHA
  = Cipher { cipherID          = 0xC013
           , cipherName        = "ECDHE-RSA-AES128CBC-SHA"
           , cipherBulk        = bulk_aes128
           , cipherHash        = SHA1
           , cipherPRFHash     = Nothing
           , cipherKeyExchange = CipherKeyExchange_ECDHE_RSA
           , cipherMinVer      = Just TLS10
           }

cipher_ECDHE_RSA_AES256CBC_SHA :: Cipher
cipher_ECDHE_RSA_AES256CBC_SHA
  = Cipher { cipherID          = 0xC014
           , cipherName        = "ECDHE-RSA-AES256CBC-SHA"
           , cipherBulk        = bulk_aes256
           , cipherHash        = SHA1
           , cipherPRFHash     = Nothing
           , cipherKeyExchange = CipherKeyExchange_ECDHE_RSA
           , cipherMinVer      = Just TLS10
           }

bulk_aes128 :: Bulk
bulk_aes128
  = Bulk { bulkName       = "AES128"
         , bulkKeySize    = 16
         , bulkIVSize     = 16
         , bulkExplicitIV = 0
         , bulkAuthTagLen = 0
         , bulkBlockSize  = 16
         , bulkF          = BulkBlockF aes128cbc
         }

bulk_aes256 :: Bulk
bulk_aes256
  = Bulk { bulkName       = "AES256"
         , bulkKeySize    = 32
         , bulkIVSize     = 16
         , bulkExplicitIV = 0
         , bulkAuthTagLen = 0
         , bulkBlockSize  = 16
         , bulkF          = BulkBlockF aes256cbc
         }

noFail :: CryptoFailable a -> a
noFail = throwCryptoError

makeIV_ :: BlockCipher a => B.ByteString -> IV a
makeIV_ = fromMaybe (error "makeIV_") . makeIV

takelast :: Int -> B.ByteString -> B.ByteString
takelast i b = B.drop (B.length b - i) b

aes128cbc :: BulkDirection -> BulkKey -> BulkBlock
aes128cbc BulkEncrypt key
  = let
      ctx = noFail (cipherInit key) :: AES128
    in
      (\iv input -> let
           output = cbcEncrypt ctx (makeIV_ iv) input
         in
           ( output, takelast 16 output ))
aes128cbc BulkDecrypt key
  = let
      ctx = noFail (cipherInit key) :: AES128
    in
      (\iv input -> let
           output = cbcDecrypt ctx (makeIV_ iv) input
         in
           ( output, takelast 16 input ))

aes256cbc :: BulkDirection -> BulkKey -> BulkBlock
aes256cbc BulkEncrypt key
  = let
      ctx = noFail (cipherInit key) :: AES256
    in
      (\iv input -> let
           output = cbcEncrypt ctx (makeIV_ iv) input
         in
           ( output, takelast 16 output ))
aes256cbc BulkDecrypt key
  = let
      ctx = noFail (cipherInit key) :: AES256
    in
      (\iv input -> let
           output = cbcDecrypt ctx (makeIV_ iv) input
         in
           ( output, takelast 16 input ))