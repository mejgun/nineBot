import           Test.Hspec

import qualified Lib.PostContent               as PostContent
import qualified Lib.Inet.Impl.Dummy           as Inet.Impl.Dummy
import qualified Lib.Inet                      as Inet

main :: IO ()
main = do
  inetH <- Inet.Impl.Dummy.newHandle
  hspec $ describe "PostContent.getPost" $ do

    it "returns 9gag post"
      $              PostContent.getPost inetH "9gag.com/123"
      `shouldReturn` nineGagResult

    it "returns 9gag post"
      $              PostContent.getPost inetH "http://9gag.com/123"
      `shouldReturn` nineGagResult

    it "returns 9gag post"
      $              PostContent.getPost inetH "https://9gag.com/123"
      `shouldReturn` nineGagResult

    it "returns 9gag post"
      $              PostContent.getPost inetH "https://m.9gag.com/123"
      `shouldReturn` nineGagResult

    it "returns 9gag post"
      $              PostContent.getPost inetH "http://m.9gag.com/123"
      `shouldReturn` nineGagResult

    it "returns 9gag post"
      $              PostContent.getPost inetH "m.9gag.com/123"
      `shouldReturn` nineGagResult

    it "returns vk post"
      $              PostContent.getPost inetH "vk.com/123"
      `shouldReturn` vkResult

    it "returns vk post"
      $              PostContent.getPost inetH "m.vk.com/123"
      `shouldReturn` vkResult

    it "returns vk post"
      $              PostContent.getPost inetH "http://vk.com/123"
      `shouldReturn` vkResult

    it "returns vk post"
      $              PostContent.getPost inetH "https://vk.com/123"
      `shouldReturn` vkResult

    it "returns vk post"
      $              PostContent.getPost inetH "https://m.vk.com/123"
      `shouldReturn` vkResult

    it "returns vk post"
      $              PostContent.getPost inetH "http://m.vk.com/123"
      `shouldReturn` vkResult

    it "returns error on unknown site"
      $              PostContent.getPost inetH "error.com/123"
      `shouldReturn` (Left PostContent.UnknownSite)


nineGagResult =
  (Right PostContent.Resp
    { PostContent.video   = []
    , PostContent.photo   =
      ["https://img-9gag-fun.9cache.com/photo/a9nMveL_700b.jpg"]
    , PostContent.caption = "We’re agile now because Jira"
    , PostContent.url     = "http://9gag.com/gag/a9nMveL"
    }
  )

vkResult =
  (Right PostContent.Resp
    { PostContent.photo   =
      [ "https://sun1-89.userapi.com/e6CMJ5qBeGvJVA9qDg4tQ7nL_FAvOfTQBYPy3A/7-IP24BJTRQ.jpg"
      , "https://sun1-93.userapi.com/OmK3FYarOnbcrA7lRT5oS2W-ZDdmkO4wuYEziA/nlQi3EfM6Uk.jpg"
      , "https://sun1-94.userapi.com/oCXEDhtsI1vfeU2V1au05bclJ1yYwfr3B4qndA/iH6RD039nOk.jpg"
      , "https://sun1-88.userapi.com/Y9_y9jdO4yTvTcdCBlfPEN_Zh0OaTPrU5NUa6A/7qZmQ4uEE7s.jpg"
      , "https://sun1-93.userapi.com/k3uNga_2hYIGpXHZMqzXGGK9zFLmk6UMjIganA/efejfA1Mobs.jpg"
      , "https://sun1-84.userapi.com/ONo-qB9xQ6gGfBMl8-gYo81683wfj8c6fsYf4A/LMs1xX7Y_LI.jpg"
      , "https://sun1-91.userapi.com/__1KNnLbx5iszIqFMcpEu3TVdsDHuzbFHRg5lg/MurpeK-_CE8.jpg"
      , "https://sun1-24.userapi.com/vWNypD28E-Hbv9qTw-KvdsOLBMahVSRDpFVOCA/aee0r8QBb20.jpg"
      , "https://sun1-24.userapi.com/qyBPsKx2YqbSS32EwQJ_fk_mfsq8usk8IgOF3Q/e0X3htJx0Bs.jpg"
      ]
    , PostContent.video   = []
    , PostContent.caption = "Сиpени многo не бывaет"
    , PostContent.url     = "https://vk.com/wall-52537634_1543772"
    }
  )
