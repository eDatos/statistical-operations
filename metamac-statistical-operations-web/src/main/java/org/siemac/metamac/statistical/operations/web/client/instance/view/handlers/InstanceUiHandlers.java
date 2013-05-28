package org.siemac.metamac.statistical.operations.web.client.instance.view.handlers;

import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.statistical.operations.web.client.view.handlers.ExternalResourcesUiHandlers;

public interface InstanceUiHandlers extends ExternalResourcesUiHandlers {

    void saveInstance(InstanceDto instanceDto);

    void populateStatisticalUnitConcepts(String schemeUri);
}
