import Application
import Network.Wai.Handler.Warp

main = withSite (run 3000)