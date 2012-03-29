package org.siemac.metamac.gopestat.web.client.utils;

import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.FamilyDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.InstanceDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationBaseDto;
import org.siemac.metamac.gopestat.core.dto.serviceapi.OperationDto;
import org.siemac.metamac.gopestat.web.client.GopestatWeb;
import org.siemac.metamac.gopestat.web.client.model.FamilyRecord;
import org.siemac.metamac.gopestat.web.client.model.InstanceRecord;
import org.siemac.metamac.gopestat.web.client.model.OperationRecord;
import org.siemac.metamac.web.common.client.utils.InternationalStringUtils;

public class RecordUtils {

    /**
     * Returns {@link FamilyRecord} from {@link FamilyBaseDto}
     * 
     * @param familyBaseDto
     * @return
     */
    public static FamilyRecord getFamilyRecord(FamilyBaseDto familyBaseDto) {
        FamilyRecord record = new FamilyRecord(familyBaseDto.getId(), familyBaseDto.getCode(), InternationalStringUtils.getLocalisedString(familyBaseDto.getTitle()),
                InternationalStringUtils.getLocalisedString(familyBaseDto.getDescription()), GopestatWeb.getCoreMessages().getString(
                        GopestatWeb.getCoreMessages().procStatusEnum() + familyBaseDto.getProcStatus().getName()));
        return record;
    }

    /**
     * Returns {@link FamilyRecord} from {@link FamilyDto}
     * 
     * @param familyDto
     * @return
     */
    public static FamilyRecord getFamilyRecord(FamilyDto familyDto) {
        FamilyRecord record = new FamilyRecord(familyDto.getId(), familyDto.getCode(), InternationalStringUtils.getLocalisedString(familyDto.getTitle()),
                InternationalStringUtils.getLocalisedString(familyDto.getDescription()), GopestatWeb.getCoreMessages().getString(
                        GopestatWeb.getCoreMessages().procStatusEnum() + familyDto.getProcStatus().getName()));
        return record;
    }

    /**
     * Returns {@link OperationRecord} from {@link OperationBaseDto}
     * 
     * @param operationBaseDto
     * @return
     */
    public static OperationRecord getOperationRecord(OperationBaseDto operationBaseDto) {
        OperationRecord record = new OperationRecord(operationBaseDto.getId(), operationBaseDto.getCode(), InternationalStringUtils.getLocalisedString(operationBaseDto.getTitle()),
                InternationalStringUtils.getLocalisedString(operationBaseDto.getAcronym()), GopestatWeb.getCoreMessages().getString(
                        GopestatWeb.getCoreMessages().procStatusEnum() + operationBaseDto.getProcStatus().getName()));
        return record;
    }

    /**
     * Returns {@link OperationRecord} from {@link OperationDto}
     * 
     * @param operationDto
     * @return
     */
    public static OperationRecord getOperationRecord(OperationDto operationDto) {
        OperationRecord record = new OperationRecord(operationDto.getId(), operationDto.getCode(), InternationalStringUtils.getLocalisedString(operationDto.getTitle()),
                InternationalStringUtils.getLocalisedString(operationDto.getAcronym()), GopestatWeb.getCoreMessages().getString(
                        GopestatWeb.getCoreMessages().procStatusEnum() + operationDto.getProcStatus().getName()));
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceBaseDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceBaseDto instanceDto) {
        InstanceRecord record = new InstanceRecord(instanceDto.getId(), instanceDto.getCode(), InternationalStringUtils.getLocalisedString(instanceDto.getTitle()),
                InternationalStringUtils.getLocalisedString(instanceDto.getDescription()), GopestatWeb.getCoreMessages().getString(
                        GopestatWeb.getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()), instanceDto.getOrder());
        return record;
    }

    /**
     * Returns {@link InstanceRecord} from {@link InstanceDto}
     * 
     * @param instanceDto
     * @return
     */
    public static InstanceRecord getInstanceRecord(InstanceDto instanceDto) {
        InstanceRecord record = new InstanceRecord(instanceDto.getId(), instanceDto.getCode(), InternationalStringUtils.getLocalisedString(instanceDto.getTitle()), null, GopestatWeb.getCoreMessages()
                .getString(GopestatWeb.getCoreMessages().procStatusEnum() + instanceDto.getProcStatus().getName()), instanceDto.getOrder());
        return record;
    }

}
