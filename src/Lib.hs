{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Lib where

import Control.Applicative
import Prelude

import Data.Maybe
import GI.Gtk
       (widgetShowAll, onWidgetDestroy, setWindowDefaultHeight,
        setWindowDefaultWidth, setWindowTitle, boxPackStart, boxNew,
        frameSetShadowType, aspectFrameNew,
        widgetGetAllocatedHeight, widgetGetAllocatedWidth, onWidgetDraw,
        onWidgetLeaveNotifyEvent, onWidgetMotionNotifyEvent,
        widgetAddEvents, alignmentSetPadding, alignmentNew, rangeSetValue,
        scaleSetDigits, scaleSetValuePos, rangeGetValue,
        afterScaleButtonValueChanged, scaleNewWithRange, containerAdd,
        buttonBoxNew, mainQuit, onButtonActivate,
        toggleButtonGetActive, onToggleButtonToggled, buttonSetUseStock,
        toggleButtonNewWithLabel, onButtonClicked,
        buttonNewWithLabel, widgetQueueDraw, drawingAreaNew,
        windowNew, widgetDestroy, dialogRun, setAboutDialogComments,
        setAboutDialogAuthors, setAboutDialogVersion,
        setAboutDialogProgramName, aboutDialogNew, labelNew, get)
import qualified GI.Gtk as Gtk (DrawingArea(..), unsafeCastTo, Window(..)
                               , builderGetObject, builderAddFromFile, builderNew)
import GI.Cairo
import Control.Monad
import Data.IORef
import Data.List
import Data.Time
import Data.Complex
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.Rendering.Cairo
       (fill, restore, save, stroke, arc, setDash, setLineWidth, rotate, rectangle,
        setSourceRGBA, setSourceRGB, newPath, scale, translate, lineTo,
        moveTo, Render)
import qualified GI.Gtk as GI (init, main)
import GI.GLib (sourceRemove, timeoutAdd)
import GI.Gdk
       (getEventMotionY, getEventMotionX, windowGetHeight,
        windowGetWidth, getEventMotionWindow)
import GI.Gdk.Flags (EventMask(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import GI.Gtk.Enums
       (Orientation(..), WindowType(..), ShadowType(..), PositionType(..))

import Data.Monoid ((<>))
import Data.GI.Base.BasicConversions (gflagsToWord)
import Graphics.Rendering.Cairo.Types (Cairo(..))
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Internal (Render(..))



main :: IO ()
main = do

    GI.init Nothing

    builder <- Gtk.builderNew
    Gtk.builderAddFromFile builder "transparent.glade"
    mainWindow <- Gtk.builderGetObject builder "main_window" >>= Gtk.unsafeCastTo Gtk.Window . fromJust
    drawingArea <- Gtk.builderGetObject builder "drawing_area" >>= Gtk.unsafeCastTo Gtk.DrawingArea . fromJust


    screen <- mainWindow `get` #screen
    visual <- #getRgbaVisual screen
    #setVisual mainWindow visual

    onWidgetDraw drawingArea $ \(Context fp) -> withManagedPtr fp $ \p -> (`runReaderT` Cairo (castPtr p)) $ runRender $ do
      w <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth drawingArea
      h <- liftIO $ fromIntegral <$> widgetGetAllocatedHeight drawingArea
      renderBG w h
      return True

    setWindowTitle mainWindow "TransparentHs"
    onWidgetDestroy mainWindow mainQuit
    widgetShowAll mainWindow

    GI.main

renderBG :: Double -> Double -> Render ()
renderBG w h = do
--  save
  setSourceRGBA 0.1953125 0.203125 0 0.6640625
  rectangle 0 0 w h
  fill
--  restore
