<script>
  import { createEventDispatcher } from 'svelte';
  import Map from './Map.svelte';

  export let selectedRegion = 'england';
  export let selectedGeography = 'england_ltla_shp';
  export let selectedAreas = [];
  export let mapConfig = {};

  const dispatch = createEventDispatcher();

  const regions = {
    nations: [
      { value: 'england', label: 'England' },
      { value: 'northern_ireland', label: 'Northern Ireland' },
      { value: 'scotland', label: 'Scotland' },
      { value: 'wales', label: 'Wales' }
    ],
    england_regions: [
      { value: 'england_central', label: 'Central' },
      { value: 'england_london', label: 'London' },
      { value: 'england_north', label: 'North' },
      { value: 'england_south', label: 'South and the Channel Islands' },
      { value: 'england_southeast', label: 'South East' }
    ]
  };

  function handleRegionChange(event) {
    selectedRegion = event.target.value;
    dispatch('regionChange', selectedRegion);
  }

  function handleGeographyChange(event) {
    selectedGeography = event.target.value;
    dispatch('geographyChange', selectedGeography);
  }

  function handleAreasChange(event) {
    selectedAreas = Array.from(event.target.selectedOptions, option => option.value);
    dispatch('areasChange', selectedAreas);
  }
</script>

<div class="selection-panel">
  <div class="select-box">
    <h4>Select a nation/region</h4>
    <select 
      class="form-control" 
      value={selectedRegion} 
      on:change={handleRegionChange}
    >
      <optgroup label="Nations">
        {#each regions.nations as region}
          <option value={region.value}>{region.label}</option>
        {/each}
      </optgroup>
      <optgroup label="Regions in England">
        {#each regions.england_regions as region}
          <option value={region.value}>{region.label}</option>
        {/each}
      </optgroup>
    </select>
  </div>

  <div class="select-box">
    <h4>Select a geography</h4>
    <select 
      class="form-control" 
      value={selectedGeography} 
      on:change={handleGeographyChange}
    >
      <option value="england_ltla_shp">Local Authorities</option>
      <!-- Add other geography options based on selected region -->
    </select>
  </div>

  <div class="select-box">
    <h4>Select up to eight areas</h4>
    <select 
      class="form-control" 
      multiple 
      value={selectedAreas} 
      on:change={handleAreasChange}
    >
      <!-- Areas will be populated based on selected geography -->
    </select>
  </div>

  <Map {mapConfig} />
</div>

<style>
  .selection-panel {
    grid-area: selection-panel;
    padding: 1rem;
  }

  .select-box {
    margin-bottom: 2rem;
    background-color: rgba(106, 158, 170, 0.188);
    padding: 1rem;
    border-radius: 4px;
  }

  .select-box h4 {
    padding: 0 0 10px 0;
    color: #193351;
  }

  .form-control {
    width: 100%;
    padding: 0.375rem 0.75rem;
    font-size: 1rem;
    line-height: 1.5;
    color: #495057;
    background-color: #fff;
    border: 1px solid #ced4da;
    border-radius: 0.25rem;
    transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
  }

  select[multiple] {
    height: auto;
    min-height: 100px;
  }
</style>