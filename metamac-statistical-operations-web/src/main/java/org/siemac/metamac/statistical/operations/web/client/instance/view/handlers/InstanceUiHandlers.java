package org.siemac.metamac.statistical.operations.web.client.instance.view.handlers;

import org.siemac.metamac.statistical.operations.core.dto.InstanceDto;
import org.siemac.metamac.web.common.client.view.handlers.SrmExternalResourcesUiHandlers;

public interface InstanceUiHandlers extends SrmExternalResourcesUiHandlers {

    void saveInstance(InstanceDto instanceDto);
    void deleteInstance(InstanceDto instanceDto);
}
