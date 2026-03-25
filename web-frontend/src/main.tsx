import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import './index.css'
import App from './App.tsx'
import SamplePage from './pages/SamplePage.tsx'

const pathname = window.location.pathname.replace(/\/+$/, '') || '/'
const RootComponent = pathname === '/sample' ? SamplePage : App

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <RootComponent />
  </StrictMode>,
)
