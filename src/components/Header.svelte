<script>
  import Banner from './Banner.svelte';
</script>

<header class="header">
  <h1><b>Health Inequalities Explorer</b></h1>
  
  <div class="updates">
    <b>Recent updates (June 2024):</b>
    <ul>
        <li>Access to healthcare indicators broken down to physical and digital access</li>
    </ul>
    
    <b>March 2024:</b>
    <ul>
        <li>Added ONS Health Index Domains and Sub-domains for England's Local Authorities.</li>
        <li>Added filter for British Red Cross regions.</li>
    </ul>
  </div>
  
  <Banner type="phase" text="ALPHA" />
  <p>This is a new service - new data and features are being added.</p>
  
  <h4>
    <b>Use this interactive tool to explore health statistics and demographics in your local area.</b>
  </h4>
  
  <div class="user-guide">
    <a href="https://medium.com/insight-and-improvement-at-british-red-cross/health-inequalities-explorer-f77025a2f1a3" 
       target="_blank" 
       rel="noopener noreferrer">
      <button class="guide-button">SEE QUICK USER GUIDE</button>
    </a>
  </div>
</header>

<style>
  .header {
    grid-area: header;
    border-bottom: 2px solid #262626;
    padding: 1rem 0;
  }

  .updates {
    margin: 1rem 0;
  }

  .text-primary {
    color: #1d70b8;
  }

  .guide-button {
    color: #212529;
    font-weight: 700;
    font-size: 2.45rem;
    letter-spacing: 1px;
    background-color: #f8f9fa;
    border: 1px solid #E0E0E0;
    margin-top: 1rem;
    padding: 0.5rem 1.5rem;
    border-radius: 50px;
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .guide-button:hover {
    background-color: #f8f9fa;
    border-color: #717171;
    box-shadow: rgba(0, 0, 0, .1) 0 2px 1px;
  }

  .guide-button:active {
    outline: none;
    transform: translateY(2px);
  }

  h4 {
    margin: 1rem 0;
  }
</style>